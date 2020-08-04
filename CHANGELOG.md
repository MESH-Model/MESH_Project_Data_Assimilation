# MESH-DATA-ASSIMILATION software is an open source software 
# is provided for geoscience researchers. 

SEE THE BOTTOM OF THIS FILE FOR THE MOST RECENT CHANGES!

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+ Mar  23 2017 :: rma_fixed_filenames merge changes.  Revision: 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Specific namelist changes include:

- Adding using land_force_perturb, forcepert_types, file_variables modules 
					  - Adding local variables tt, N_t, n2 
					  - Adding three forcing data perturbation fields precip_pert, sw_pert, lw_pert
					  - Adding reading input variables of the Input2.ini file 
					  - Adding ensemble loop 
					  - Adding the section of Initialize random_fields variables  
					  - Adding the section of generate random fields for initialization
					  - Adding the section of assigning variables related to perturbation fields 
					  - Adding the section of perturbing input forcing data for every time step 
						   by considering the spatial-temporal and cross correlation. 
					  - Modifying the gridded data variables for output 





++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+ MMM DD YYYY :: summary of changes in next Manhattan update       Tag: vX.Y.Z
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
