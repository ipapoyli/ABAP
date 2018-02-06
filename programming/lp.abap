*******************************************************************************
*   GENIOS – an LP Solver in ABAP
*******************************************************************************



DATA:
  lr_model       TYPE REF TO cl_genios_model, "problem instance
  lr_objective   TYPE REF TO cl_genios_objective, "objective function
  lr_environment TYPE REF TO cl_genios_environment,
  lr_x1          TYPE REF TO cl_genios_variable, "variables
  lr_x2          TYPE REF TO cl_genios_variable,
  lr_x3          TYPE REF TO cl_genios_variable,
  lr_constraint  TYPE REF TO cl_genios_linearconstraint,
  lr_solver      TYPE REF TO cl_genios_solver,
  ls_result      TYPE genioss_solver_result,
  ls_variable    TYPE genioss_variable,
  lt_variables   TYPE geniost_variable,
  lv_value       TYPE genios_float,
  lv_name        TYPE string. "name of the variable

lr_environment = cl_genios_environment=>get_environment( ).
lr_model = lr_environment->create_model( 'PRIORIZATION' ). "model name
* Do maximazation
lr_objective = lr_model->create_objective(
  if_genios_model_c=>gc_obj_maximization ).
* We have three continous variables x1, x2 and x3
lr_x1 = lr_model->create_variable( iv_name = 'x1'
  iv_type = if_genios_model_c=>gc_var_continuous ).
lr_x2 = lr_model->create_variable( iv_name = 'x2'
  iv_type = if_genios_model_c=>gc_var_continuous ).
lr_x3 = lr_model->create_variable( iv_name = 'x3'
  iv_type = if_genios_model_c=>gc_var_continuous ).
* Define objective function as 15 x1 + 10 x2 + 40 x3
lr_objective->add_monom( io_variable = lr_x1 iv_coefficient = 15 ).
lr_objective->add_monom( io_variable = lr_x2 iv_coefficient = 10 ).
lr_objective->add_monom( io_variable = lr_x3 iv_coefficient = 40 ).
* Definition of linear constraints: x1 <= 100
lr_constraint = lr_model->create_linearconstraint( iv_name = 'c1'
  iv_type = if_genios_model_c=>gc_con_lessorequal iv_righthandside = 100 ).
lr_constraint->add_monom( io_variable = lr_x1 iv_coefficient = 1 ).
* Definition of linear constraints: x2 <= 50
lr_constraint = lr_model->create_linearconstraint( iv_name = 'c2'
  iv_type = if_genios_model_c=>gc_con_lessorequal iv_righthandside = 50 ).
lr_constraint->add_monom( io_variable = lr_x2 iv_coefficient = 1 ).
* Definition of linear constraints: x3 <= 40
lr_constraint = lr_model->create_linearconstraint( iv_name = 'c3'
  iv_type = if_genios_model_c=>gc_con_lessorequal iv_righthandside = 40 ).
lr_constraint->add_monom( io_variable = lr_x3 iv_coefficient = 1 ).
* Definition of linear constraints: 10 x1 + 50 x2 + 80 x3 <= 2000
lr_constraint = lr_model->create_linearconstraint( iv_name = 'c4'
  iv_type = if_genios_model_c=>gc_con_lessorequal iv_righthandside = 2000 ).
lr_constraint->add_monom( io_variable = lr_x1 iv_coefficient= 10 ).
lr_constraint->add_monom( io_variable = lr_x2 iv_coefficient = 50 ).
lr_constraint->add_monom( io_variable = lr_x3 iv_coefficient = 80 ).
* Use the simplex solver
lr_solver = lr_environment->create_solver( 'SIMP' ).
lr_solver->load_model( lr_model ). "load the model into the solver
ls_result = lr_solver->solve( ).   "solve the problem
* Get the result
IF ls_result-solution_status = if_genios_solver_result_c=>gc_optimal OR
   ls_result-solution_status = if_genios_solver_result_c=>gc_abortfeasible.
  lt_variables = lr_model->get_variables( ).
  LOOP AT lt_variables INTO ls_variable.
    lv_name  = ls_variable-variable_ref->gv_name.
    lv_value = ls_variable-variable_ref->get_primalvalue( ).
    WRITE: /,lv_name,' = ',lv_value.
  ENDLOOP.
ENDIF.
lr_environment->destroy_solver( 'SIMP' ).
lr_environment->destroy_model( 'PRIORIZATION' ).
