
include .\subroutines\fcastdecomp_subroutines.prg

' ##################################################################################################################
' ##################################################################################################################
' ###################################################### SETTINGS ###################################################
' ##################################################################################################################
' ##################################################################################################################

' 1. Detemine if  this was run from the GUI
!dogui=1
if @len(@option(1))>0 then
	!dogui=@hasoption("prompt") 'if this is 0, we are NOT running through the GUI
endif

' 2. Specifyin settings

if !dogui=1 then

	'2.1 Execute user dialog
	%fd_eq_name = _this.@name

	%fd_scenarios = ""

	!fd_include_addf = 0

	%fd_sample = @pagesmpl
	%fd_graph_name = "gp_fd"

	!fd_keep_table = 0

	@uidialog("caption","Forecast decomposition settings", _
		"edit",%fd_alias_list,"Enter scenario alias(es)", _
		"check",!fd_include_addf,"Include add-factors", _
		"edit",%fd_sample,"Enter graph sample", _
		"edit",%fd_graph_name,"Enter graph name", _		 
		"check",!fd_keep_table,"Store the decomposition table")

	for %set include_addf keep_table
		if !fd_{%set}=1 then
			%fd_{%set} = "T"
		else
			%fd_{%set} = "F"
		endif
	next

	%fd_use_table = "f"

else

	'2.2 Load settings from options
	%fd_eq_name = _this.@name

	%fd_alias_list = @equaloption("ALIAS_LIST")
		
	%fd_include_addf = @equaloption("INCLUDE_ADDF")	 'scenario specific add-factors by default?

	if @isempty(%fd_include_addf) then
		%fd_include_addf= "f"
	endif

	%fd_sample =  @equaloption("SAMPLE")	
	
	if @isempty(%fd_sample) then
		%fd_sample = @pagesmpl
	endif

	%fd_graph_name = @equaloption("GRAPH_NAME")

	%fd_use_table = @equaloption("USE_TABLE")

	if @isempty(%fd_use_table) then
		%fd_use_table = "f"
	endif
	
	%fd_keep_table = @equaloption("KEEP_TABLE")	
	
	if @isempty(%fd_keep_table) then
		%fd_keep_table = "f"
	endif
endif

' ##################################################################################################################
' ##################################################################################################################
' #################################################### EXECUTION ###################################################
' ##################################################################################################################
' ##################################################################################################################

if @upper(%fd_use_table)="F" or @isobject("tb_forecast_decomposition")=0 then
	call forecast_decomposition_table(%fd_eq_name,%fd_include_addf)
endif

if tb_forecast_decomposition.@rows>1 then
	call forecast_decomposition_graph("tb_forecast_decomposition",%fd_alias_list ,%fd_sample,%fd_graph_name)
endif
 
delete(noerr)  sc_coef st_driver  m_fd st_eq_varlist st_eq_spec  gr_regs st_graph_string

if @upper(%fd_keep_table)<>"T" then
	delete(noerr) tb_forecast_decomposition st_equation_vars
endif


