' ##################################################################################################################
' ##################################################################################################################
' ############################ SUBROUTINES FOR FORECAST DECOMPOSITION ADD-IN ######################################
' ##################################################################################################################
' ##################################################################################################################

' ##################################################################################################################

subroutine forecast_decomposition_table(string %eq_name, string %sub_include_addf, string %sub_include_sum)

' 1. Settings

%sub_model_name =  "m_fd"

delete(noerr) tb_forecast_decomposition
table tb_forecast_decomposition

tb_forecast_decomposition(1,1) = "Driver number"
tb_forecast_decomposition(1,2) = "Stand-alone driver"
tb_forecast_decomposition(1,3) = "Difference driver"
 
' 2. Creating model object

{%eq_name}.makemodel({%sub_model_name}) 

' 3. Creating group of equation variables
string st_eq_varlist = " " + {%eq_name}.@varlist + " "
string st_eq_spec = " " + {%eq_name}.@spec + " "
string st_equation_vars = @upper({%sub_model_name}.@varlist)

' Removing instruments, PDLs and other problematic terms
if @instr(st_eq_varlist," @ ")>0 then
	st_eq_varlist  = @left(st_eq_varlist,@instr(st_eq_varlist," @ "))
endif 

' 3. Creating group of equation variables
string st_equation_vars = @upper({%sub_model_name}.@varlist)

' 4. Creating group of drivers
if {%eq_name}.@bylist<>0 then
	call drivers_table(%eq_name, %sub_include_addf, %sub_include_sum)
else
	@uiprompt("Forecast decomposition does not work for equations estimated by explicit formula.")
endif

endsub

' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

subroutine drivers_table(string %eq_name, string %sub_include_addf, string %sub_include_sum)

!pdl_driver_present = 0
!driver_n = 0
!coef_order = 0

for !d= 1 to  @wcount(st_eq_varlist)

	' Creating driver string
	string st_driver = @word(st_eq_varlist,!d)

	if @upper(st_driver)<>"C" then

		!driver_n = !driver_n +1

		' Dealing with PDL drivers
		if @left(st_driver,3)="PDL" then
			tb_forecast_decomposition(1+!driver_n,1) = "DELETE"
			!pdl_driver_present = 1
		endif
		
		' Identifying coefficient
		if !d>1 and @left(st_driver,3)<>"PDL" then
			!coef_order = !coef_order + 1
			scalar sc_coef  = {%eq_name}.@coef(!coef_order) 
	
			call formated_coef(sc_coef,"coef")

		endif

		' Adding scenario aliases 
		for %es {st_equation_vars}
			st_driver = @replace(st_driver,%es,%es + "_salias")		
		next
	
		' Storing stand alone driver
		if !d=1 then
			tb_forecast_decomposition(1+!driver_n,2) =  st_driver
		else
			tb_forecast_decomposition(1+!driver_n,2) = %coef+ "*(" + st_driver + ")"
		endif
	
		' Storing difference driver
		if !d=1 then
			tb_forecast_decomposition(1+!driver_n,3) =  @replace(st_driver,"salias","salias1")  + "-" + "("  +  @replace(st_driver,"salias","salias2") + ")"
		else
			tb_forecast_decomposition(1+!driver_n,3) =    %coef + "*("  + @replace(st_driver,"salias","salias1")  + "-" + "("+@replace(st_driver,"salias","salias2") + ")" + ")"
		endif
	else
		!coef_order = !coef_order + 1
		!constant_driver = !d
	endif
next

' Adding pdl drivers
if !pdl_driver_present = 1 then
	call pdl_drivers(%eq_name)
endif

' Adding driver numbers

for !d= 1 to  tb_forecast_decomposition.@rows-1
	if !d=1 then
		tb_forecast_decomposition(1+!d,1) = "DV"
	else
		tb_forecast_decomposition(1+!d,1) = @str(!d-1,"f.0")
	endif
next

' Adding constant
if @instr(@upper(st_eq_varlist)," C ")>0 then

	tb_forecast_decomposition.deleterow(1+!driver_n+1) 1
	tb_forecast_decomposition.insertrow(3) 1 

	scalar sc_coef = {%eq_name}.@coef(!constant_driver -1)
	tb_forecast_decomposition(3,1) =  "0"

	call formated_coef(sc_coef,"coef")
	tb_forecast_decomposition(3,2) =  %coef

endif

' Adding add-factor

if @upper(%sub_include_addf)="T" then

	%dv = @word(st_eq_varlist,1)
	group gr_dv {%dv}
	%target = @word(gr_dv.@depends,1)

	tb_forecast_decomposition(2+!driver_n +1,1) = "ADDF"
	tb_forecast_decomposition(2+!driver_n +1,2) = %target + "_a" + "_salias"
	tb_forecast_decomposition(2+!driver_n +1,3) =  %target + "_a" +"_salias1" + "-" + %target + "_a" +  "_salias2" 
endif	

' Adding driver sum
if @upper(%sub_include_sum)="T" then 

	!sum_row = tb_forecast_decomposition.@rows+1

	for !dtc = 2 to 3
		%driver_sum = ""
		for !d=1 to !driver_n
			%driver_sum = %driver_sum  + tb_forecast_decomposition(1+!d+1,!dtc)  + "+"
		next
	
		%driver_sum = @left(%driver_sum,@length(%driver_sum)-1)
	
		tb_forecast_decomposition(!sum_row,1) = "SUM"
		tb_forecast_decomposition(!sum_row,!dtc) = %driver_sum
	next

endif

endsub

' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

subroutine pdl_drivers(string %eq_name)

' Eliminating rows corresponding to PDL drivers

for !tr = 2 to tb_forecast_decomposition.@rows
	if tb_forecast_decomposition(!tr,1) = "DELETE" then
		tb_forecast_decomposition.deleterow(!tr) 1 
		!tr=!tr-1
	endif
next

!driver_count = tb_forecast_decomposition.@rows-1

' Identifying PDL drivers

!pdl_driver_count = 0

for !w = 1 to @wcount(st_eq_spec)

	string st_reg_string = @word(st_eq_spec,!w)

	if @instr(@upper(st_reg_string),"PDL")>0 then

		!pdl_driver_count = !pdl_driver_count+1

		' 1. Identifying properties of PDL term	
		scalar sc_bracket_count = (@strlen(st_reg_string)-@strlen(@replace(st_reg_string,")","")))/@strlen(")")

		if sc_bracket_count>1 then
			string st_pdl_parameter_string =@mid(st_reg_string,@instr(st_reg_string,")",sc_bracket_count-1)+1)
			st_pdl_parameter_string =@mid(st_pdl_parameter_string,@instr(st_pdl_parameter_string,",",1)+1)
		else	
			string st_pdl_parameter_string =@mid(st_reg_string,@instr(st_reg_string,",",1)+1)
		endif

		!ending_cut = @length(st_pdl_parameter_string)+1
		!begining_cut = 4							

		!pdl_lag = @val(@left(st_pdl_parameter_string,@instr(st_pdl_parameter_string,",")-1))

		if @instr(st_pdl_parameter_string,",",2)>0 then
			string st_pdl_degree =@mid(st_pdl_parameter_string,@instr(st_pdl_parameter_string,",")+1)
			!pdl_degree = @val(@left(st_pdl_degree,@instr(st_pdl_degree,",")-1))

			%pdl_constraint = @mid(st_pdl_parameter_string,@instr(st_pdl_parameter_string,",",2)+1)
			!pdl_constraint = @val(@left(%pdl_constraint,@instr(st_pdl_degree,",")-1))
		else
			string st_pdl_degree =@mid(st_pdl_parameter_string,@instr(st_pdl_parameter_string,",")+1)
			!pdl_degree = @val(@left(st_pdl_degree,@length(st_pdl_degree)-1))

			!pdl_constraint = 0
		endif

		' Identifying  
		%pdl_driver_{!pdl_driver_count} = @mid(st_reg_string,!begining_cut +1,@length(st_reg_string)-!begining_cut-!ending_cut)

		if @right((%pdl_driver_{!pdl_driver_count}),1)<>")" then
			!ending_cut = 6+2
			%pdl_driver_{!pdl_driver_count} = @mid(st_reg_string,!begining_cut +1,@length(st_reg_string)-!begining_cut-!ending_cut)
		endif
		
		%pdl_driver_{!pdl_driver_count} = @replace(%pdl_driver_{!pdl_driver_count}," ","")

		' 2. Creating PDL string
		string st_pdl_driver = %pdl_driver_{!pdl_driver_count} 
		string st_pdl_driver_lag = st_pdl_driver	

		for %es {st_equation_vars}
			st_pdl_driver_lag = @replace(st_pdl_driver_lag,%es,%es + "(-lag)")		
		next

		string st_pdl_driver_full = ""

		for !lag = 1 to !pdl_lag
		
			sc_coef = {%eq_name}.@coef( {%eq_name}.@ncoefs+!lag) 

			st_pdl_driver_full = st_pdl_driver_full  + @str(sc_coef,"g.2") + "*" + @replace(st_pdl_driver_lag,"lag",@str(!lag)) + "+"
		next

		for %es {st_equation_vars}
			st_pdl_driver_full = @replace(st_pdl_driver_full,%es, %es + "_salias")		
		next

		st_pdl_driver_full = @left(st_pdl_driver_full,@length(st_pdl_driver_full)-1)

		string st_pdl_driver_full_difference = "(" + @replace(st_pdl_driver_full,"salias","salias1") + ")" + "-" + "(" + @replace(st_pdl_driver_full,"salias","salias2") + ")"
		

		tb_forecast_decomposition(1+!driver_count+!pdl_driver_count,1) = @str(!driver_count+!pdl_driver_count,"f.0")
		tb_forecast_decomposition(1+!driver_count+!pdl_driver_count,2) = st_pdl_driver_full	
		tb_forecast_decomposition(1+!driver_count+!pdl_driver_count,3) = st_pdl_driver_full_difference	
		
	endif
next

endsub

' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

subroutine formated_coef(scalar !sub_value,string %sub_string_name)

if @abs(!sub_value)>10 then
	%{%sub_string_name}= @str(!sub_value,"f.0")
endif

if @abs(!sub_value)<10 and @abs(!sub_value)>1 then
	%{%sub_string_name}= @str(!sub_value,"f.2")
endif

if @abs(!sub_value)<1 then
	%{%sub_string_name}= @str(!sub_value,"g.2")
endif

if @isna(!sub_value) then
	%{%sub_string_name}= @str(!sub_value)
endif

endsub

' ##################################################################################################################



' ##################################################################################################################

subroutine forecast_decomposition_graph(string %tb_name,string %sub_fd_scenarios, string %sub_fd_sample, string %sub_fd_graph_name, string %sub_include_addf, string %sub_include_sum)

'1. Creating graph string

' Dealing with separate alias for 
for !sub_alias = 1 to @wcount(%sub_fd_scenarios)
	%sub_alias = @word(%sub_fd_scenarios,!sub_alias)

	if @instr(@upper(%sub_alias),"[")>0 then
		%primary_alias{!sub_alias} = @left(%sub_alias,@instr(%sub_alias,"[")-1)
		%secondary_alias{!sub_alias} = @mid(%sub_alias,@instr(%sub_alias,"[")+1)	
		%secondary_alias{!sub_alias} = @left(%secondary_alias{!sub_alias},@length(%secondary_alias{!sub_alias})-1)	
	else
		%primary_alias{!sub_alias} = %sub_alias
	endif
next

string st_graph_string = ""

if @wcount(%sub_fd_scenarios)=2 then
	!tc = 3
else
	!tc = 2
endif

for !tr = 2 to {%tb_name}.@rows
	
	%driver_string = {%tb_name}(!tr,!tc)

	if @wcount(%sub_fd_scenarios)>1 then
		for !salias = 1 to 2 
			%driver_string = @replace(@upper(%driver_string),"_SALIAS" + @str(!salias),%primary_alias{!salias})
		next
	else
		%driver_string = @replace(@upper(%driver_string),"_SALIAS",%primary_alias1)
	endif

	!include_driver = 1

	if {%tb_name}(!tr,1)="ADDF" and @upper(%sub_include_addf)="F" then
		!include_driver = 0
	endif 

	if {%tb_name}(!tr,1)="SUM" and @upper(%sub_include_sum)="F" then
		!include_driver = 0
	endif 

	if !include_driver = 1 then
		st_graph_string = st_graph_string  + %driver_string + " "
	endif
next

' Removing scenario alias for nonexisting scenario series 
for %es {st_equation_vars}
	for !salias = 1 to @wcount(%sub_fd_scenarios) 
		if @isobject(%es + 	%primary_alias{!salias})=0 then
			if @isobject(%es + 	%secondary_alias{!salias}) then
				st_graph_string = @replace(@upper(st_graph_string),@upper(%es + %primary_alias{!salias}),%es + %secondary_alias{!salias})
			else
				st_graph_string = @replace(@upper(st_graph_string),@upper(%es + %primary_alias{!salias}),%es)
			endif
		endif
	next
next

'2. Creating graph

if @isempty(%sub_fd_graph_name) then
	%sub_fd_graph_name = "gp_fd"
endif

if @isobject(%sub_fd_graph_name) then
	%sub_fd_graph_name = @getnextname(%sub_fd_graph_name)
endif

smpl {%sub_fd_sample}
graph {%sub_fd_graph_name}.line {st_graph_string}

' Adding legend
%legend = "Dependent variable - " +  @replace(@upper({%tb_name}(2,2)),"_SALIAS","")
{%sub_fd_graph_name}.setelem(1) legend({%legend}) symbol(filledsquare)

!e = 1 

if {%tb_name}(3,1)="0"  then

	if @wcount(%sub_fd_scenarios)<=1 then
		%legend = "Constant" 
	
		!e = !e+1
		{%sub_fd_graph_name}.setelem(!e) legend({%legend})
	endif

	!driver_row_start = 4
else
	!driver_row_start = 3
endif

for !tr = !driver_row_start  to {%tb_name}.@rows

	%legend = ""

	if {%tb_name}(!tr,1)="ADDF" then
		%legend = "Add-factor"
	endif	

	if {%tb_name}(!tr,1)="SUM" then
		%legend = "Driver sum"
	endif	

	if @isempty(%legend) then
		%legend = "Driver " + {%tb_name}(!tr,1)  + " - " +  @replace(@upper({%tb_name}(!tr,2)),"_SALIAS","")
	endif

	!e = !e+1
	{%sub_fd_graph_name}.setelem(!e) legend({%legend})
next

if !dogui=1 then
	{%sub_fd_graph_name}.display
endif

endsub

' ##################################################################################################################


