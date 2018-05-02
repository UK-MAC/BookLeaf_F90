! @HEADER@
! Crown Copyright 2018 AWE.
!
! This file is part of BookLeaf.
!
! BookLeaf is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
! 
! BookLeaf is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE. See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with
! BookLeaf. If not, see http://www.gnu.org/licenses/.
! @HEADER@
MODULE TYPH_distribute_mod

  ! Internal
  USE dataAPI_kinds_mod,     ONLY: ink,lok,rlk
  USE dataAPI_types_mod,     ONLY: config_t,sizes_t,data_t,error_t
  USE dataAPI_params_mod,    ONLY: NCORN,SUCCESS,FAILURE,HALT_SINGLE
  USE dataAPI_id_mod,        ONLY: iellocglobid,indlocglobid,ielmatid,ielndid, &
&                                  ielregid  
  USE dataAPI_comm_mod,      ONLY: keycommcells
  USE data_mod,              ONLY: data_setinit,data_setmesh,data_settyph
  USE TYPH_collect_mod,      ONLY: TYPH_reduce,TYPH_OP_SUM
  USE TYPH_decomposition_mod,ONLY: TYPH_set_partition_info
  USE TYPH_key_mod,          ONLY: TYPH_create_key_set,TYPH_KTYPE_CELL
  USE TYPH_register_mod,     ONLY: TYPH_start_register
  USE utils_kn_sort_mod,     ONLY: utils_kn_iusort,utils_kn_sort0,             &
&                                  utils_kn_sort1
  USE utils_kn_access_mod,   ONLY: utils_kn_set,utils_kn_get
  ! External
  USE mpi

  IMPLICIT NONE

  PRIVATE :: allocate_counts,deallocate_counts,set_EC_data,get_el,             &
&            get_EC_my_el_redist,set_NC_data,get_new_nodes,get_cloud_el,       &
&            get_nod2el,ns_cleanup,alloc_out,del_layinfo,set_cloud_dims,       &
&            ns_init,ns_new,ns_resize
  PUBLIC  :: TYPH_distribute_mesh

  TYPE layer_info
     INTEGER(KIND=ink) :: lay=-1_ink
     INTEGER(KIND=ink) :: nel=0_ink,nnod=0_ink
     INTEGER(KIND=ink) :: nodoff=0_ink,eloff=0_ink     
     INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: globel
     INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: globnod
     INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: conndata
  END TYPE layer_info

  TYPE(layer_info),DIMENSION(:),ALLOCATABLE,SAVE :: layinfo

  ! Node cloud data information
  INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,SAVE :: NC_data
  INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,SAVE :: NC_indx
  INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,SAVE :: NC_data_ref
  INTEGER(KIND=ink),SAVE      :: NC_global_size
  INTEGER(KIND=ink),SAVE      :: NC_size
  INTEGER(KIND=ink),SAVE      :: NC_my_size
  INTEGER(KIND=ink),SAVE      :: NC_my_offset
  INTEGER(KIND=ink),SAVE      :: NC_my_data_size
  INTEGER(KIND=ink),PARAMETER :: NC_CGNOD=1_ink
  INTEGER(KIND=ink),PARAMETER :: NC_COWNLEL=2_ink
  INTEGER(KIND=ink),PARAMETER :: NC_CGEL=3_ink
  INTEGER(KIND=ink),PARAMETER :: NC_COWNPE=4_ink
  INTEGER(KIND=ink),PARAMETER :: NC_COWNLNOD=5_ink
  INTEGER(KIND=ink),PARAMETER :: NC_LEN=5_ink

  ! Element cloud data information
  INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,SAVE :: EC_data
  INTEGER(KIND=ink),SAVE      :: EC_global_size
  INTEGER(KIND=ink),SAVE      :: EC_size
  INTEGER(KIND=ink),SAVE      :: EC_my_size
  INTEGER(KIND=ink),SAVE      :: EC_my_offset
  INTEGER(KIND=ink),PARAMETER :: EC_CGEL=1_ink
  INTEGER(KIND=ink),PARAMETER :: EC_CREG=2_ink
  INTEGER(KIND=ink),PARAMETER :: EC_CMAT=3_ink
  INTEGER(KIND=ink),PARAMETER :: EC_COWNPE=4_ink
  INTEGER(KIND=ink),PARAMETER :: EC_COWNLEL=5_ink
  INTEGER(KIND=ink),PARAMETER :: EC_CCONS=6_ink
  INTEGER(KIND=ink),SAVE      :: EC_LEN

  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,SAVE :: scounts,rcounts
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,SAVE :: sdispls,rdispls
 
  INTEGER(KIND=ink),   SAVE :: Rank,Nproc
  INTEGER(KIND=ink),   SAVE :: Comm 
  INTEGER(KIND=ink),   SAVE :: CONS,CONE,CLEN,N2ELEN
  INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,SAVE :: nodes
  INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,SAVE :: nodtoel
  INTEGER(KIND=ink),SAVE :: n2ecnt
  LOGICAL(KIND=lok)      :: zerr
  INTEGER(KIND=ink)      :: errflag
  INTEGER(KIND=ink)      :: ierr    
  INTEGER(KIND=ink)      :: reccount

  INTEGER(KIND=ink),PARAMETER :: ns_w=4_ink, ns_n=ns_w+1_ink
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,SAVE :: ns_p
  INTEGER(KIND=ink),SAVE :: ns_top=0_ink,ns_size=0_ink
  INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,SAVE :: ns_d

 CONTAINS 

  SUBROUTINE TYPH_distribute_mesh(conndata,part,config,sizes,dh,error)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: conndata 
    ! Connectivity array
    ! On INPUT:
    ! Entries are: glob el#,reg#,mat,connectivity(idx 4-7) as global IDs
    ! Deallocated on exit
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,INTENT(INOUT) :: part  
    ! intended distribution. Deallocated on exit
    TYPE(config_t),                              INTENT(IN)    :: config
    TYPE(sizes_t),                               INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),  ALLOCATABLE,INTENT(INOUT) :: dh
    TYPE(error_t),                               INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: i,k,n,ip,ii,lay,kk,reccount,nel,nel_glob,nnd_glob,nlay
    INTEGER(KIND=ink) :: wholemesh
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: nghst_el,nghst_nd,Ne_tot,  &
&                                                   Nn_tot
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: sdata,rdata,indownerproc,  &
&                                                   ielownerproc

    CLEN=SIZE(conndata,1) ! Connectivity table length
    CONS=4_ink
    CONE=CLEN
    EC_LEN=CLEN
    ! Get processor topology
    Nproc=config%comm%spatial%nproc
    Rank=config%comm%spatial%rank
    Comm=config%comm%spatial%comm
    ! Global mesh size
    nel_glob=sizes%nel
    nnd_glob=sizes%nnd
    ! Number of ghost layers
    IF (config%ale%zexist) THEN
      nlay=2_ink
    ELSE
      nlay=1_ink
    ENDIF

    ! Allocate node list variable nodes     
    ALLOCATE(nodes(NCORN))     

    ! Allocate scounts, rcounts, sdispls, rdispls (0:Nproc-1)
    CALL allocate_counts(Nproc,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Allocate layinfo(0:nlay), layer_info structure for each layer
    ALLOCATE(layinfo(0:nlay))

    ! layinfo 0
    nel=SIZE(conndata,2)
    ALLOCATE(layinfo(0)%conndata(CLEN+2,nel))
    DO i=1,nel
      layinfo(0)%conndata(1:3,i)=conndata(1:3,i)
      layinfo(0)%conndata(CONS+2_ink:CONE+2_ink,i)=conndata(CONS:CONE,i)
    ENDDO
    DEALLOCATE(conndata)
    CONS=CONS+2_ink
    CONE=CONE+2_ink
    CLEN=CLEN+2_ink
    EC_LEN=CLEN
    CALL utils_kn_sort0(layinfo(0)%conndata)
    DO i=1,nel
      layinfo(0)%conndata(EC_COWNPE,i)=part(i)
      layinfo(0)%conndata(EC_COWNLEL,i)=0_ink
    ENDDO
    DEALLOCATE(part)

    ! Set layinfo(0)
    layinfo(0)%lay=0_ink
    layinfo(0)%nel=nel

    ! Send conndata to the element cloud (EC)
    CALL set_EC_data(layinfo(0),Comm,nel_glob,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! When redistributing mesh, replace layinfo(0)%conndata with 
    ! connectivity data for this PE
    ! Create local element numbering and send back to EC
    CALL get_EC_my_el_redist(layinfo(0))
       
    ! Get list of global elements layinfo(0)%globel
    CALL get_el(layinfo(0))
    
    ! Compute expanded node to element connectivity  (nodtoel) and 
    ! send to node cloud (NC)
    CALL set_NC_data(layinfo(0),nnd_glob)

    ! Get list of global nodes layinfo(0)%globnod
    ! and convert layinfo(0)%conndata connectivity to local node 
    ! numbering
    CALL get_new_nodes(linfo=layinfo(0),error=error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Deallocate node to element connectivity table
    DEALLOCATE(nodtoel)

    ! Loop over ghost layer, expanding mesh for each
    DO lay=1,nlay
      ! Get connectivity for elements surrounding layer lay-1 
      ! (layinfo(lay)%conndata)
      CALL get_cloud_el(layinfo(lay),layinfo(lay-1))
      ! Get list of global elements layinfo(lay)%globel
      CALL get_el(layinfo(lay))
      ! Get node to element connectivity (nodtoel) for new elements 
      ! (layinfo(lay)%conndata)
      CALL get_nod2el(layinfo(lay))
      ! Get list of NEW global nodes layinfo(lay)%globnod
      ! and convert layinfo(lay)%conndata connectivity to local node 
      ! numbering
      CALL get_new_nodes(layinfo(lay),layinfo(lay-1),error)
      IF (error%ierr.NE.SUCCESS) RETURN
      DEALLOCATE(nodtoel)
    ENDDO
    ! Cleanup data structures to track which elements have already
    ! been returned by NC to this PE in get_cloud_el
    CALL ns_cleanup()

    ! Finally use layinfo data to produce final output
    sizes%nel=layinfo(0)%nel
    sizes%nnd=layinfo(0)%nnod
    sizes%nel2=SUM(layinfo%nel)
    sizes%nnd2=SUM(layinfo%nnod)
   
    ! Initialise data setup
    CALL data_setinit(sizes,dh)
    ! Start typhon registry
    ierr=TYPH_start_register()
    ! Set mesh data
    CALL data_setmesh(sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    ! Set typhon data
    CALL data_settyph(sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Allocate variables for typhon setup
    CALL alloc_out(nlay,sizes%nel2,sizes%nnd2,nghst_el,nghst_nd,Ne_tot,Nn_tot, &
&                  ielownerproc,indownerproc)

    ! Set variables
    DO lay=0,nlay
      nghst_el(lay)=layinfo(lay)%nel
      nghst_nd(lay)=layinfo(lay)%nnod
      DO i=1,layinfo(lay)%nel
        ii=i+layinfo(lay)%eloff
        CALL utils_kn_set(ii,sizes%nel2,layinfo(lay)%globel(i),                &
&                         dh(iellocglobid)%iaddr)
        CALL utils_kn_set(ii,sizes%nel2,layinfo(lay)%conndata(EC_CREG,i),      &
&                         dh(ielregid)%iaddr)
        CALL utils_kn_set(ii,sizes%nel2,layinfo(lay)%conndata(EC_CMAT,i),      &
&                         dh(ielmatid)%iaddr)
        DO k=1,NCORN
          kk=CONS+k-1
          CALL utils_kn_set((ii-1_ink)*NCORN+k,NCORN*sizes%nel2,               &
&                           layinfo(lay)%conndata(kk,i),dh(ielndid)%iaddr)
        ENDDO
      ENDDO
    ENDDO
    DO i=1,layinfo(0)%nel
      ii=i+layinfo(0)%eloff
      ielownerproc(1,ii)=Rank  
      ielownerproc(2,ii)=ii
    ENDDO
    Ne_tot=nghst_el
    Nn_tot=nghst_nd
    DO lay=1,nlay
      Ne_tot(lay)=Ne_tot(lay)+Ne_tot(lay-1)
      Nn_tot(lay)=Nn_tot(lay)+Nn_tot(lay-1)
      DO i=1,layinfo(lay)%nel
        ii=i+layinfo(lay)%eloff
        ielownerproc(1,ii)=layinfo(lay)%conndata(EC_COWNPE,i) 
        ielownerproc(2,ii)=layinfo(lay)%conndata(EC_COWNLEL,i) 
      ENDDO
    ENDDO
    DO lay=0,nlay
      DO i=1,layinfo(lay)%nnod
        ii=i+layinfo(lay)%nodoff
        CALL utils_kn_set(ii,sizes%nnd2,layinfo(lay)%globnod(i),               &
&                         dh(indlocglobid)%iaddr)
      ENDDO
    ENDDO

    ! Construct sdata(1,:) a list of global nodes we need information for
    ! Send each part to the appropriate PE in NC
    scounts=0_ink               
    DO i=1,sizes%nnd2
      n=utils_kn_get(i,sizes%nnd2,dh(indlocglobid)%iaddr)
      ip=(n-1_ink)/NC_size
      scounts(ip)=scounts(ip)+1_ink
    ENDDO
    sdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
      sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
    ENDDO
    ALLOCATE(sdata(2,sizes%nnd2))
    DO i=1,sizes%nnd2
      n=utils_kn_get(i,sizes%nnd2,dh(indlocglobid)%iaddr)
      ip=(n-1_ink)/NC_size
      sdispls(ip)=sdispls(ip)+1_ink
      sdata(1,sdispls(ip))=n
      sdata(2,sdispls(ip))=0_ink
    ENDDO
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    reccount=SUM(rcounts)
    scounts(0)=scounts(0)*2_ink
    rcounts(0)=rcounts(0)*2_ink
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
      scounts(ip)=scounts(ip)*2_ink
      rcounts(ip)=rcounts(ip)*2_ink
      sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
      rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO 
    ALLOCATE(rdata(2,reccount))
    CALL MPI_alltoallv(sdata,scounts,sdispls,MPI_INTEGER,rdata,rcounts,rdispls,&
&                      MPI_INTEGER,Comm,ierr)
    ! Loop over receive list rdata and set source PE and corresponding  
    ! source local node number for each entry
    DO i=1,reccount
      n=rdata(1,i)-NC_my_offset
      ii=NC_indx(n+1)-1_ink ! Last element surrounding node with have   
                            ! greatest global element number as sorted 
                            ! by global node number and then global 
                            ! element number
      rdata(1,i)=NC_data(NC_COWNPE,ii)
      rdata(2,i)=NC_data(NC_COWNLNOD,ii)
    ENDDO
    ! Send back data to requesting PE, into sdata
    CALL MPI_alltoallv(rdata,rcounts,rdispls,MPI_INTEGER,sdata,scounts,sdispls,&
&                      MPI_INTEGER,Comm,ierr)     
    DEALLOCATE(rdata)

    ! Loop over sdata and set nodtoproc values
    scounts(0)=scounts(0)/2_ink
    sdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
      scounts(ip)=scounts(ip)/2_ink
      sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
    ENDDO
    DO i=1,sizes%nnd2
      n=utils_kn_get(i,sizes%nnd2,dh(indlocglobid)%iaddr)
      ip=(n-1_ink)/NC_size
      sdispls(ip)=sdispls(ip)+1_ink
      indownerproc(1,i)=sdata(1,sdispls(ip)) 
      indownerproc(2,i)=sdata(2,sdispls(ip))
    ENDDO
    DEALLOCATE(sdata)

    SELECT CASE(nlay)
      CASE(1_ink)
        sizes%nel1=sizes%nel2
        sizes%nnd1=sizes%nnd2
      CASE(2_ink)
        sizes%nel1=Ne_tot(1)
        sizes%nnd1=Nn_tot(1)
      CASE DEFAULT
        error%ierr=FAILURE
        error%iout=HALT_SINGLE
        error%serr="ERROR: incorrect value for nlay"
        RETURN
    END SELECT

    ! Deallocate cloud data
    DEALLOCATE(NC_indx,NC_data,NC_data_ref,EC_data)

    ! Deallocate layinfo
    DO lay=0,nlay
      CALL del_layinfo(layinfo(lay))
    ENDDO
    DEALLOCATE(layinfo)

    ! Deallocate processor count variables
    CALL deallocate_counts(Nproc,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Deallocate node list variable
    DEALLOCATE(nodes)
    ! Setup typhon communication
    ierr=TYPH_set_partition_info(WHOLEMESH,4_ink,nlay,Ne_tot,Nn_tot,           &
&                                ielownerproc,indownerproc,                    &
&                                dh(iellocglobid)%iaddr,dh(indlocglobid)%iaddr,&
&                                dh(ielndid)%iaddr)
    ierr=TYPH_create_key_set(keycommcells,TYPH_KTYPE_CELL,1_ink,nlay,WHOLEMESH)

    ! Deallocate local arrays
    DEALLOCATE(nghst_el,nghst_nd,Ne_tot,Nn_tot,ielownerproc,indownerproc)

  END SUBROUTINE TYPH_distribute_mesh

  SUBROUTINE alloc_out(nlay,nel2,nnod2,nghst_el,nghst_nd,Ne_tot,Nn_tot,        &
&                      ielownerproc,indownerproc)

    ! Argument list
    INTEGER(KIND=ink),                           INTENT(IN)  :: nlay,nel2,nnod2
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,INTENT(OUT) :: nghst_el,      &
&                                                               nghst_nd,      &
&                                                               Ne_tot,Nn_tot
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(OUT) :: ielownerproc,  &
&                                                               indownerproc
    
    ALLOCATE(nghst_el(0:nlay))
    ALLOCATE(nghst_nd(0:nlay))
    ALLOCATE(Ne_tot(0:nlay))
    ALLOCATE(Nn_tot(0:nlay))
    ALLOCATE(ielownerproc(2,nel2))
    ALLOCATE(indownerproc(2,nnod2))

  END SUBROUTINE alloc_out

  SUBROUTINE del_layinfo(linfo)

    ! Deallocate any arrays in linfo
    TYPE(layer_info) :: linfo

    DEALLOCATE(linfo%globel)
    DEALLOCATE(linfo%globnod)
    DEALLOCATE(linfo%conndata)

  END SUBROUTINE del_layinfo

  SUBROUTINE get_el(linfo)

    TYPE(layer_info)  :: linfo
    INTEGER(KIND=ink) :: i

    ! Get global list of elements linfo%globel from linfo%conndata
    ! which is assumed to be sorted in increasing global element number 
    linfo%nel=SIZE(linfo%conndata,2)
    ALLOCATE(linfo%globel(linfo%nel))
    DO i=1,linfo%nel
       linfo%globel(i)=linfo%conndata(EC_CGEL,i)
    ENDDO

  END SUBROUTINE get_el

  SUBROUTINE set_EC_data(linfo,Comm,nel_glob,error)

    TYPE(layer_info)  :: linfo
    INTEGER(KIND=ink) :: Comm
    INTEGER(KIND=ink) :: nel_glob
    TYPE(error_t)     :: error
    INTEGER(KIND=ink) :: i,j,ip,reccount,cnt
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: rdata

    ! Set element cloud data from linfo%conndata

    ! Determine cloud dimensions
    CALL set_cloud_dims(nel_glob,Nproc,Rank,EC_size,EC_my_size,EC_my_offset)

    ! Determine how much data is to be sent to each PE in cloud
    scounts=0_ink                 
    DO i=1,linfo%nel
       j=linfo%conndata(EC_CGEL,i)
       ip=(j-1_ink)/EC_size
       scounts(ip)=scounts(ip)+1_ink
    END DO
    ! Data is already ordered correctly as linfo%conndata is sorted in global 
    ! element number

    ! Determine receive counts
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    reccount=SUM(rcounts)     
    ALLOCATE(rdata(EC_LEN,reccount))

    ! Send data 
    scounts=scounts*EC_LEN
    rcounts=rcounts*EC_LEN
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO     
    CALL MPI_alltoallv(linfo%conndata,scounts,sdispls,MPI_INTEGER,rdata,       &
&                      rcounts,rdispls,MPI_INTEGER,Comm,ierr)

    ! allocate EC data to my size
    ALLOCATE(EC_data(EC_LEN,EC_my_size))

    ! Set EC data from rdata       
    EC_data=0_ink
    cnt=0_ink ! number of instance found where cell data has already been set
    DO i=1,reccount
       j=rdata(EC_CGEL,i)-EC_my_offset
       IF (EC_data(EC_CGEL,j)/=0_ink) cnt=cnt+1_ink
       EC_data(:,j)=rdata(:,i)
    ENDDO
          
    DEALLOCATE(rdata)

    ! Sum cnt across PEs
    ierr=TYPH_reduce(cnt,i,TYPH_OP_SUM,Comm)
    IF (Rank==0_ink.AND.i.GT.0_ink) THEN
       ! Error if any cell connectivity is duplicated, indicate invalid mesh 
       ! partitioning
       error%ierr=FAILURE
       error%iout=HALT_SINGLE
       error%serr='ERROR: Elements have been found on more than one partition'
       RETURN
    ENDIF

  END SUBROUTINE set_EC_data

  SUBROUTINE get_EC_my_el_redist(linfo)

    TYPE(layer_info)  :: linfo
    INTEGER(KIND=ink) :: i,ip,sendcount,j
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: sconn,sdata,rdata

    ! Called if parent routine has to redistribute mesh
    ! Get connectivity table from EC based on target PE in EC table (entry 3)

    ! Deallocate current connectivity data
    DEALLOCATE(linfo%conndata)     

    ! EC_data(EC_COWNPE,:) are the target PEs 
    ! Determine how much data is to be sent to each PE
    scounts=0_ink
    DO i=1,EC_my_size
       IF (EC_data(EC_CGEL,i).GT.0_ink) THEN
          ip=EC_data(EC_COWNPE,i)
          scounts(ip)=scounts(ip)+1_ink
       ENDIF
    ENDDO
    sendcount=SUM(scounts)

    ! Allocate array for connectivity data to be sent
    ALLOCATE(sconn(EC_LEN,sendcount))

    ! Fill sconn
    sdispls(0)=0_ink     
    DO ip=1,Nproc-1_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
    ENDDO
    DO i=1,EC_my_size
       IF (EC_data(EC_CGEL,i).GT.0_ink) THEN
          ip=EC_data(EC_COWNPE,i)
          sdispls(ip)=sdispls(ip)+1_ink
          sconn(:,sdispls(ip))=EC_data(:,i)
       ENDIF
    ENDDO

    ! Determine receive counts
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    linfo%nel=SUM(rcounts)

    ! Allocate new connectivity array linfo%conndata
    ALLOCATE(linfo%conndata(EC_LEN,linfo%nel))

    ! Send in sconn receive in linfo%conndata
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    scounts(0)=scounts(0)*EC_LEN
    rcounts(0)=rcounts(0)*EC_LEN
    DO ip=1,Nproc-1_ink
       scounts(ip)=scounts(ip)*EC_LEN
       rcounts(ip)=rcounts(ip)*EC_LEN
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    END DO     
    CALL MPI_alltoallv(sconn,scounts,sdispls,MPI_INTEGER,linfo%conndata,       &
&                      rcounts,rdispls,MPI_INTEGER,Comm,ierr)    

    ! Deallocate sconn
    DEALLOCATE(sconn)

    ! Sort linfo%conndata by global element number (column 1)
    CALL utils_kn_sort0(linfo%conndata)

    ! Determine local element numbers
    DO i=1,linfo%nel
      linfo%conndata(EC_COWNLEL,i)=i
      linfo%conndata(EC_COWNPE,i)=Rank
    END DO

    ! Allocate array sdata to send local element numbers to EC
    ALLOCATE(sdata(2,linfo%nel))

    ! Array of global, local element numbers, already sorted by destination PE 
    ! in EC
    scounts=0_ink
    DO i=1,linfo%nel
      j=linfo%conndata(EC_CGEL,i)
      sdata(1,i)=j
      sdata(2,i)=i
      ip=(j-1_ink)/EC_size
      scounts(ip)=scounts(ip)+1_ink
    END DO
    sdispls(0)=0_ink     

    ! Determine rcounts
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    reccount=SUM(rcounts)

    ! Allocate receive array rdata
    ALLOCATE(rdata(2,reccount))

    ! Send in sdata, receive in rdata
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    scounts(0)=scounts(0)*2_ink
    rcounts(0)=rcounts(0)*2_ink
    DO ip=1,Nproc-1_ink
       scounts(ip)=scounts(ip)*2_ink
       rcounts(ip)=rcounts(ip)*2_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    END DO     
    CALL MPI_alltoallv(sdata,scounts,sdispls,MPI_INTEGER,rdata,rcounts,rdispls,&
&                      MPI_INTEGER,Comm,ierr)    

    ! Deallocate send array sdata
    DEALLOCATE(sdata)

    ! Loop over rdata entries and set local element numbers
    DO i=1,reccount
       j=rdata(EC_CGEL,i)-EC_my_offset
       EC_data(EC_COWNLEL,j)=rdata(2,i)
    ENDDO

    ! Deallocate receive array rdata
    DEALLOCATE(rdata)

  END SUBROUTINE get_EC_my_el_redist

  SUBROUTINE set_NC_data(linfo,nnd_glob)

    TYPE(layer_info),                            INTENT(IN)    :: linfo
    INTEGER(KIND=ink),                           INTENT(IN)    :: nnd_glob
    INTEGER(KIND=ink) :: i,j,cnt,n,k,prev,jg,ip,ii,iii,n2ecnt2
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: itmp

    ! Construct extended node to element connectivity and send nodal
    ! data to node cloud NC

    N2ELEN=NC_LEN
    n2ecnt=0_ink
    ! Determine node to element table length n2ecnt
    ! Loop over elements in connectivity table
    DO j=1,linfo%nel
       nodes(1:NCORN)=linfo%conndata(CONS:CONE,j) ! Get list of nodes in element
                                                  ! connectivity
       CALL utils_kn_iusort(nodes,cnt) ! There are cnt unique nodes in
                                       ! element connectivity
       n2ecnt=n2ecnt+cnt ! Add to total
    ENDDO

    ! Allocate node to element connectivity table
    ALLOCATE(nodtoel(N2ELEN,n2ecnt))

    n2ecnt=0_ink ! Total number of rows
    n2ecnt2=0_ink ! Count number of positive node entries
    ! Loop over elements in connectivity table     
    DO j=1,linfo%nel
       nodes(1:NCORN)=linfo%conndata(CONS:CONE,j) ! Get list of nodes in element
                                                  ! connectivity
       CALL utils_kn_iusort(nodes,cnt) ! Get the cnt unique nodes in
                                       ! element connectivity
       jg=linfo%conndata(EC_CGEL,j)
       ! Create entry for each of the cnt unique nodes
       DO k=1,cnt
          n=nodes(k)
          n2ecnt=n2ecnt+1_ink
          IF (n.GT.0_ink) THEN
             n2ecnt2=n2ecnt2+1_ink
             nodtoel(NC_CGNOD,n2ecnt)=n ! The global node number
             nodtoel(NC_COWNLEL,n2ecnt)=j ! The local element number
          ENDIF
          nodtoel(NC_CGEL,n2ecnt)=jg ! The global element number
          nodtoel(NC_COWNPE,n2ecnt)=Rank ! The source PE (this PE)
          nodtoel(NC_COWNLNOD,n2ecnt)=0_ink ! Will hold the local node number later
       ENDDO
    ENDDO
    CALL utils_kn_sort0(nodtoel) ! Sort by global node number
    ! Set the local node numbers
    prev=-1_ink
    cnt=0_ink  
    DO i=1,n2ecnt
       n=nodtoel(NC_CGNOD,i)
       IF (n.NE.prev) THEN
          cnt=cnt+1_ink 
          prev=n
       ENDIF
       nodtoel(NC_COWNLNOD,i)=cnt
    ENDDO
    
    ! Set NC dimensions
    CALL set_cloud_dims(nnd_glob,Nproc,Rank,NC_size,NC_my_size,NC_my_offset)

    IF (n2ecnt2.LT.n2ecnt) THEN
      ! There rows corresponding to negative node references
      ! Transfer only n2ecnt2 the positive rows of nodtoel to new array itmp, 
      ! which is used instead of nodtoel
      ALLOCATE(itmp(N2ELEN,n2ecnt2))
      cnt=0_ink
      DO i=1,n2ecnt
         j=nodtoel(NC_COWNLEL,i)
         IF (j.GT.0_ink) THEN
            cnt=cnt+1_ink
            itmp(:,cnt)=nodtoel(:,i)
         ENDIF
      ENDDO
      ! Determine the number of entries to be sent to each PE in NC
      scounts=0_ink                 
      DO i=1,n2ecnt2
         n=itmp(NC_CGNOD,i)
         ip=(n-1_ink)/NC_size
         scounts(ip)=scounts(ip)+1_ink
      ENDDO
      ! These are already stored in the correct order as sorted by global 
      ! node number
    ELSE
      ! Determine the number of entries to be sent to each PE in NC
      scounts=0_ink                 
      DO i=1,n2ecnt
         n=nodtoel(NC_CGNOD,i)
         ip=(n-1_ink)/NC_size
         scounts(ip)=scounts(ip)+1_ink
      ENDDO
      ! These are already stored in the correct order as sorted by global node number
    ENDIF

    ! Determine receive count
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    ! This is the amount of data to be stored in NC on this PE
    NC_my_data_size=SUM(rcounts)

    ! Allocated NC data arrays
    ALLOCATE(NC_data(NC_LEN,NC_my_data_size))   
    ALLOCATE(NC_data_ref(NC_my_data_size)) 
    NC_data_ref=0_ink

    ! Send in nodtoel, receive in NC_data
    scounts=scounts*NC_LEN
    rcounts=rcounts*NC_LEN
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO     

    IF (n2ecnt2.LT.n2ecnt) THEN
       ! Communicare either itmp or nodtoel
       CALL MPI_alltoallv(itmp,scounts,sdispls,MPI_INTEGER,NC_data,rcounts,    &
&                         rdispls,MPI_INTEGER,Comm,ierr)
       DEALLOCATE(itmp)
    ELSE
       CALL MPI_alltoallv(nodtoel,scounts,sdispls,MPI_INTEGER,NC_data,rcounts, &
&                         rdispls,MPI_INTEGER,Comm,ierr)
    ENDIF

    ! Sort by global element number and set a unique reference number of 
    ! element received
    CALL utils_kn_sort1(NC_data,(/NC_CGEL/))
    prev=-1_ink
    cnt=0_ink
    DO i=1,NC_my_data_size
       j=NC_data(NC_CGEL,i)
       IF (j.NE.prev) THEN
          cnt=cnt+1_ink
          prev=j
       ENDIF
       NC_data_ref(i)=cnt
    ENDDO
    ! cnt is the largest reference number, i.e. there are cnt unique elements in NC
    ! Initialise ns_ routines to store references from 1 to cnt. These routines 
    ! determine if a particular reference has been previously sent to a 
    ! particular PE number
    CALL ns_init(cnt)

    ! Sort by global node number and global element number, applying the same
    ! permutation to NC_data_ref
    CALL utils_kn_sort1(NC_data,(/NC_CGNOD,NC_CGEL/),NC_data_ref)

    ! Create index of node data on NC on this PE
    ALLOCATE(NC_indx(NC_my_size+1))
    ! Count number element entries for each node
    NC_indx=0_ink
    DO i=1,NC_my_data_size
       n=NC_data(NC_CGNOD,i)-NC_my_offset
       NC_indx(n)=NC_indx(n)+1_ink
    ENDDO
    ! Construct index array
    ii=1_ink
    DO i=1,NC_my_size
       iii=ii+NC_indx(i)
       NC_indx(i)=ii
       ii=iii
    ENDDO
    NC_indx(i)=ii
    
  END SUBROUTINE set_NC_data

  SUBROUTINE get_nod2el(linfo)

    TYPE(layer_info)  :: linfo
    INTEGER(KIND=ink) :: j,cnt,n,k

    ! Create node to element connectivity array from linfo%conndata

    ! Determine node to element table length n2ecnt     
    N2ELEN=2_ink
    n2ecnt=0_ink
    ! Loop over elements in connectivity table     
    DO j=1,linfo%nel
       nodes(1:NCORN)=linfo%conndata(CONS:CONE,j)! Get list of nodes in element
                                                 ! connectivity
       CALL utils_kn_iusort(nodes,cnt) ! There are cnt unique nodes in
                                       ! element connectivity
       n2ecnt=n2ecnt+cnt ! Add to total
    ENDDO

    ! Allocate node to element connectivity table     
    ALLOCATE(nodtoel(N2ELEN,n2ecnt))

    n2ecnt=0_ink
    ! Loop over elements in connectivity table          
    DO j=1,linfo%nel
       nodes(1:NCORN)=linfo%conndata(CONS:CONE,j) ! Get list of nodes in element
                                                  ! connectivity
       CALL utils_kn_iusort(nodes,cnt) ! Get the cnt unique nodes in
                                       ! element connectivity
       ! Create entry for each of the cnt unique nodes
       DO k=1,cnt
          n=nodes(k)
          n2ecnt=n2ecnt+1_ink
          nodtoel(NC_CGNOD,n2ecnt)=n ! The global node number
          nodtoel(NC_COWNLEL,n2ecnt)=j ! The local element number
       ENDDO
    ENDDO
    CALL utils_kn_sort0(nodtoel) ! Sort by global node number     

  END SUBROUTINE get_nod2el

  SUBROUTINE get_new_nodes(linfo,linfo0,error)

    TYPE(layer_info)          :: linfo
    TYPE(layer_info),OPTIONAL :: linfo0
    TYPE(error_t),INTENT(OUT) :: error
    INTEGER(KIND=ink) :: i,ii,n,cnt,ilast,prev,iloc,nn,nnod0,j,k

    ! Determine nodes in nodtoel that are not in linfo0%globnod
    ! and create new local node numbers for these. 
    ! Make use of the fact that both nodtoel and linfo0%globnod are sorted
    ! Modify the connectivity linfo%conndata to be in terms of the new local 
    ! numbering

    ! Can handle the case where linfo0 is not set
    nnod0=0_ink
    IF (PRESENT(linfo0)) nnod0=linfo0%nnod

    prev=-1_ink
    cnt=0_ink  
    ilast=1_ink     

    DO i=1,SIZE(nodtoel,2)
      n=nodtoel(NC_CGNOD,i) ! The global node number
      IF (n.NE.prev) THEN ! This is a new node
        nn=n
        DO ii=ilast,nnod0 ! look for node in linfo0%globnod
          IF (n.LE.linfo0%globnod(ii)) EXIT
        ENDDO
        IF (ii.LE.nnod0) THEN
          IF (n.EQ.linfo0%globnod(ii)) THEN
            ! Node found in linfo0%globnod
            iloc=linfo0%nodoff+ii ! This is the local node number of this node
            ii=ii+1_ink
            nn=-n
          ELSE
            ! This is is a new node, create new local number
            cnt=cnt+1_ink
            iloc=linfo%nodoff+cnt
          ENDIF
        ELSE
          ! Gone off the end of linfo0%globnod
          ! This is is a new node, create new local number
          cnt=cnt+1_ink
          iloc=linfo%nodoff+cnt
        ENDIF
        ilast=ii ! Start point in linfo0%globnodnext time around
        prev=n
      ENDIF
      ! This is the local element number
      j=nodtoel(NC_COWNLEL,i)
      ! Replace any occurances of node n in connectivity with -iloc the local 
      ! node number
      IF (j.GT.0_ink) THEN
        DO k=CONS,CONE
          IF (linfo%conndata(k,j)==n) linfo%conndata(k,j)=-iloc
        ENDDO
      ENDIF
      ! If this is not a new node then nn=-n, otherwise nn=n
      ! This will enable us to easily identify the new nodes later
      nodtoel(NC_CGNOD,i)=nn
    ENDDO

    linfo%nnod=cnt
    ALLOCATE(linfo%globnod(linfo%nnod))
    cnt=0_ink
    prev=-1_ink
    DO i=1,SIZE(nodtoel,2)
      n=nodtoel(NC_CGNOD,i)
      IF ((n.GT.0_ink).AND.(n.NE.prev)) THEN
        cnt=cnt+1_ink
        linfo%globnod(cnt)=n
        prev=n
      ENDIF
    ENDDO
    cnt=0_ink
    DO i=1,SIZE(linfo%conndata,2)
      DO k=CONS,CONE
        IF (linfo%conndata(k,j).GT.0_ink) cnt=cnt+1_ink
      ENDDO
    ENDDO
    IF (cnt.GT.0_ink) THEN
      error%ierr=FAILURE
      error%iout=HALT_SINGLE
      error%serr='ERROR: Local nodes not found in connectivity for layer'
      RETURN
    ENDIF
    DO i=1,SIZE(linfo%conndata,2)
       DO k=CONS,CONE
         linfo%conndata(k,i)=-linfo%conndata(k,i)
       ENDDO
    ENDDO

  END SUBROUTINE get_new_nodes
  
  SUBROUTINE get_cloud_el(linfo,linfo0)

    TYPE(layer_info)  :: linfo,linfo0
    INTEGER(KIND=ink) :: i,n,ip,ii,ipp,scnt,iii,nel0,cnt,prev,ilast,j,jj
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: slist,rlist
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: sconn

    ! Get from NC the elements surrounding linfo0%globnod(i)
    ! Does not return element on the same processor or elements that
    ! have been previously returned to this process

    linfo%lay=linfo0%lay+1_ink
    linfo%nodoff=linfo0%nodoff+linfo0%nnod
    linfo%eloff=linfo0%eloff+linfo0%nel

    ! Loop over linfo0%globnod(i) and count how may got to each PE in NC
    scounts=0_ink
    DO i=1,linfo0%nnod
       n=linfo0%globnod(i)
       ip=(n-1_ink)/NC_size
       scounts(ip)=scounts(ip)+1_ink
    ENDDO
    ! Get receive counts
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    reccount=SUM(rcounts)
    ! Allocate receive list rlist
    ALLOCATE(rlist(reccount))

    ! Send in linfo0%globnod recvieve in rlist
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO     
    CALL MPI_alltoallv(linfo0%globnod,scounts,sdispls,MPI_INTEGER,rlist,       &
&                      rcounts,rdispls,MPI_INTEGER,Comm,ierr)
    scnt=0_ink  
    ii=0_ink
    ! Loop over receive nodes by processor and count potential number of  
    ! elements to be sent
    DO ip=0,Nproc-1_ink
       DO i=ii+1,ii+rcounts(ip)
          n=rlist(i)-NC_my_offset
          ! Loop over element entries for node
          DO iii=NC_indx(n),NC_indx(n+1)-1
             ipp=NC_data(NC_COWNPE,iii)
             IF (ip.NE.ipp) THEN
                ! Not sending back to the source process
                scnt=scnt+1_ink
             ENDIF
          ENDDO
       ENDDO
       ii=ii+rcounts(ip)
    ENDDO
    ! Allocate send list
    ALLOCATE(slist(scnt))
    slist=0_ink
    scounts=0_ink
    scnt=0_ink
    ii=0_ink
    ! Loop over receive nodes by processor
    DO ip=0,Nproc-1_ink
       DO i=ii+1,ii+rcounts(ip)
          n=rlist(i)-NC_my_offset
          ! Loop over element entries for node
          DO iii=NC_indx(n),NC_indx(n+1)-1
             ipp=NC_data(NC_COWNPE,iii)
             IF (ip.NE.ipp) THEN
                ! Not sending back to the source process
                IF (ns_new(ip,NC_data_ref(iii))) THEN
                   ! Element with reference NC_data_ref(iii) has not been sent
                   ! to PE ip before, so must send to PE ip. 
                   ! comms_ns_new(ip,NC_data_ref(iii)) will now record element
                   ! with reference NC_data_ref(iii) as having been sent to PE.
                   scounts(ip)=scounts(ip)+1_ink
                   scnt=scnt+1_ink
                   slist(scnt)=NC_data(NC_CGEL,iii)
                ENDIF
             ENDIF
          ENDDO
       ENDDO
       ii=ii+rcounts(ip)
    ENDDO
    DEALLOCATE(rlist)

    ! Determine receive counts
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    reccount=SUM(rcounts)

    ! Allocate receive list rlist
    ALLOCATE(rlist(reccount))

    ! Send in slist, receive in rlist
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO      
    CALL MPI_alltoallv(slist,scounts,sdispls,MPI_INTEGER,rlist,rcounts,rdispls,&
&                      MPI_INTEGER,Comm,ierr)
  
    ! Deallocate send list slist
    DEALLOCATE(slist)

    ! Sort by globel element number
    CALL utils_kn_sort0(rlist)

    ! Determine list of unique new elements (not in linfo0%globel)
    ! Makes use of fact the at both rlist and linfo0%globel) are in increasing 
    ! order
    nel0=linfo0%nel
    cnt=0_ink
    prev=-1_ink
    ilast=1_ink
    ! Loop over elements received
    DO i=1,reccount
       j=rlist(i) ! Global element number
       jj=0_ink
       IF (j.NE.prev) THEN
          ! Different element, look for element in linfo0%globel
          DO ii=ilast,nel0
             IF (j.LE.linfo0%globel(ii)) EXIT
          ENDDO
          IF (ii.LE.nel0) THEN
             IF (j.EQ.linfo0%globel(ii)) THEN
                ! j has been found as linfo0%globel(ii)
                ii=ii+1_ink
             ELSE
                ! j is not in linfo0%globel
                ! Count as new local element
                jj=j
                cnt=cnt+1_ink
             ENDIF
          ELSE
             ! Gone off the end of linfo0%globel
             ! j is not in linfo0%globel
             ! Count as new local element
             jj=j
             cnt=cnt+1_ink
          ENDIF
          ilast=ii ! Starting point in linfo0%globel for next time around
          prev=j
       ENDIF
       rlist(i)=jj ! jj=j if j is a new element, otherwise jj=0
                   ! Will allow us to indentfy new nodes in next part
    ENDDO
    linfo%nel=cnt ! Set number of new elements
    ! Create list of the new element
    ! Determine how many are sent to each PE in the EC
    ALLOCATE(slist(linfo%nel))
    cnt=0_ink
    scounts=0_ink     
    DO i=1,reccount
       IF (rlist(i).GT.0_ink) THEN
          cnt=cnt+1_ink
          j=rlist(i)
          slist(cnt)=j
          ip=(j-1_ink)/EC_size
          scounts(ip)=scounts(ip)+1_ink
       ENDIF
    ENDDO
    ! Deallocate rlist
    DEALLOCATE(rlist)

    ! Determine receive counts
    CALL MPI_alltoall(scounts,1_ink,MPI_INTEGER,rcounts,1_ink,MPI_INTEGER,Comm,&
&                     ierr)
    reccount=SUM(rcounts)
    ! Allocate recive list rlist
    ALLOCATE(rlist(reccount))

    ! Send in slist, receive in rlist
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    DO ip=1,Nproc-1_ink
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO     
    CALL MPI_alltoallv(slist,scounts,sdispls,MPI_INTEGER,rlist,rcounts,rdispls,&
&                      MPI_INTEGER,Comm,ierr)

    ! Deallocates slist
    DEALLOCATE(slist)

    ! Allocate array to send connectivity information
    ALLOCATE(sconn(EC_LEN,reccount))
    ! Set connectivity information to be sent
    DO i=1,reccount
       sconn(:,i)=EC_data(:,rlist(i)-EC_my_offset)
    ENDDO

    ! Deallocate rlist
    DEALLOCATE(rlist)

    ! Allocate linfo%conndata for final connectivity data
    ALLOCATE(linfo%conndata(EC_LEN,linfo%nel))

    ! send back connectivity information
    ! Send in sconn, receive in linfo%conndata
    ! Scaled reverse of slist/rlist transfer above
    sdispls(0)=0_ink
    rdispls(0)=0_ink
    scounts(0)=scounts(0)*EC_LEN
    rcounts(0)=rcounts(0)*EC_LEN
    DO ip=1,Nproc-1_ink
       scounts(ip)=scounts(ip)*EC_LEN
       rcounts(ip)=rcounts(ip)*EC_LEN
       sdispls(ip)=sdispls(ip-1)+scounts(ip-1)
       rdispls(ip)=rdispls(ip-1)+rcounts(ip-1)
    ENDDO     
    CALL MPI_alltoallv(sconn,rcounts,rdispls,MPI_INTEGER,linfo%conndata,       &
&                      scounts,sdispls,MPI_INTEGER,Comm,ierr)

    ! Deallocate sconn send array
    DEALLOCATE(sconn)

    ! Sort linfo%conndata by global element number
    CALL utils_kn_sort0(linfo%conndata)

  END SUBROUTINE get_cloud_el

  SUBROUTINE set_cloud_dims(globsize,npes,myrank,cloud_size,my_cloud_size,     &
&                           my_cloud_offset)

    INTEGER(KIND=ink),INTENT(IN)  :: globsize,npes,myrank
    INTEGER(KIND=ink),INTENT(OUT) :: cloud_size,my_cloud_size,my_cloud_offset

    ! Compute cloud dimensions
    cloud_size=(globsize-1_ink)/npes+1_ink
    my_cloud_offset=myrank*cloud_size
    my_cloud_size=MAX(MIN(globsize-my_cloud_offset,cloud_size),0_ink) 

  END SUBROUTINE set_cloud_dims

  SUBROUTINE allocate_counts(Nproc,error)

    INTEGER(KIND=ink),INTENT(IN)  :: Nproc
    TYPE(error_t),    INTENT(OUT) :: error

    ! Allocate processor counts and displacements
    ALLOCATE(scounts(0:Nproc-1),rcounts(0:Nproc-1),sdispls(0:Nproc-1),         &
&            rdispls(0:Nproc-1),STAT=errflag)
    IF (errflag.NE.0_ink) THEN
      error%ierr=FAILURE
      error%iout=HALT_SINGLE
      error%serr='ERROR: Allocation error: scounts(0:Nproc-1),....'
      RETURN
    ENDIF

  END SUBROUTINE allocate_counts

  SUBROUTINE deallocate_counts(Nproc,error)

    INTEGER(KIND=ink),INTENT(IN)  :: Nproc 
    TYPE(error_t),    INTENT(OUT) :: error

    ! Deallocate processor counts and displacements
    DEALLOCATE(scounts,rcounts,sdispls,rdispls,STAT=errflag)
    IF (errflag.NE.0_ink) THEN
      error%ierr=FAILURE
      error%iout=HALT_SINGLE
      error%serr='ERROR: Deallocation error: scounts(0:Nproc-1),....'
      RETURN
    ENDIF

  END SUBROUTINE deallocate_counts

  FUNCTION ns_new(ipd,indx)

    LOGICAL(KIND=lok) :: ns_new
    INTEGER(KIND=ink),INTENT(IN) :: ipd,indx
    INTEGER(KIND=ink) :: k,ii,itmp,kk,itmp2,ip

    ! Input PE number 0<=ip<Nproc
    ! Reference number 1<=indx<SIZE(ns_p)
    ! Returns .FALSE. if reference has been sent to PE ip before
    ! Returns .TRUE. if reference has note been sent to PE ip before. In which 
    ! case the transfer top PE ip is recorder, so that further calls with the 
    ! same arguments will return .FALSE.
    ip=ipd+1_ink ! Actually store ip+1, so can use zero as the null value

    ns_new=.TRUE._lok
    IF (ns_p(indx).EQ.0_ink) THEN
      ! There have been no sends for this reference
      ! get and entry in ns_d to store transfer. Increase size if necessary
      IF (ns_top.GE.ns_size) CALL ns_resize(ns_top+1_ink)
      ns_top=ns_top+1_ink
      ns_p(indx)=ns_top
      ! Record transfer to ip is first entry
      ns_d(1,ns_top)=ip
      ! Nullify link to next entry in ns_d
      !ns_d(ns_n,ns_top)=0_ink
    ELSE
      ii=ns_p(indx) ! This is where the sorted list of PEs starts for this reference
      DO 
        ! Search for entry for ip
        DO k=1,ns_w
           IF ((ns_d(k,ii).EQ.0_ink).OR.(ip.LE.ns_d(k,ii))) EXIT
        ENDDO
        IF (k.LE.ns_w) THEN ! have not gone off then of ns_d(:,ii)
          IF (ip.EQ.ns_d(k,ii)) THEN 
            ! Entry found, return .FALSE. 
            ns_new=.FALSE._lok
          ELSE
            ! Entry for ip not present, will return .TRUE. 
            ! Add entry for ip and shift remaining entries forward
            itmp=ip 
            DO
              DO kk=k,ns_w
                itmp2=ns_d(kk,ii) ! Keep copy of current entry
                ns_d(kk,ii)=itmp ! Replace with last entry
                IF (itmp2==0_ink) EXIT ! Reached end of list
                itmp=itmp2
              ENDDO
              IF (kk.LE.ns_w) RETURN ! Found null entry, DONE
              ! Reached end of ns_d(:,ii)
              IF (ns_d(ns_n,ii)==0_ink) THEN
                ! No further rows linked on
                ! Link to new row, with single entry ip
                ! Increase size of ns_d if necessary
                IF (ns_top.GE.ns_size) CALL ns_resize(ns_top+1_ink)
                ns_top=ns_top+1_ink
                ns_d(1,ns_top)=itmp
                ns_d(ns_n,ii)=ns_top
                EXIT !DONE
              ENDIF
              ! Follow link to start of new row
              ii=ns_d(ns_n,ii); k=1_ink
            ENDDO
          ENDIF
          RETURN !DONE
        ENDIF
        ! Reached the end of ns_d(:,ii)
        IF (ns_d(ns_n,ii).EQ.0) THEN
          ! Will return .TRUE.
          ! No further rows linked on
          ! Link to new row, with single entry ip
          ! Increase size of ns_d if necessary
          IF (ns_top.GE.ns_size) CALL ns_resize(ns_top+1_ink)
          ns_top=ns_top+1_ink
          ns_d(1,ns_top)=ip
          ns_d(ns_n,ii)=ns_top
          EXIT
        ENDIF
        ! Follow link to start of new row
        ii=ns_d(ns_n,ii)
      ENDDO
    ENDIF

  END FUNCTION ns_new

  SUBROUTINE ns_init(siz)

    ! Initialise ns_ routines to store references from
    ! 1 to siz
    INTEGER(KIND=ink),INTENT(IN) :: siz

    ALLOCATE(ns_p(siz))
    ns_p=0_ink
    ns_top=0_ink
    ns_size=0_ink

  END SUBROUTINE ns_init

  SUBROUTINE ns_cleanup

    ! Deallocate ns_ storage
    IF (ALLOCATED(ns_p)) DEALLOCATE(ns_p)
    IF (ALLOCATED(ns_d)) DEALLOCATE(ns_d)
    ns_top=0_ink
    ns_size=0_ink

  END SUBROUTINE ns_cleanup

  SUBROUTINE ns_resize(newtop)

    INTEGER(KIND=ink),INTENT(IN) :: newtop
    INTEGER(KIND=ink),PARAMETER  :: ns_incr=10000_ink
    INTEGER(KIND=ink)            :: newsize
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: itmp

    ! Resize ns_d storage size to be at least newtop
    ! Actually newtop+ns_incr
    newsize=newtop+ns_incr
    ALLOCATE(itmp(ns_w+1,newsize))
    IF (ns_top.GT.0_ink) THEN
      itmp(:,1:ns_top)=ns_d(:,1:ns_top)
      DEALLOCATE(ns_d)
    ENDIF
    itmp(:,ns_top+1:)=0_ink
    CALL MOVE_ALLOC(itmp,ns_d)
    ns_size=newsize

  END SUBROUTINE ns_resize

END MODULE TYPH_distribute_mod
