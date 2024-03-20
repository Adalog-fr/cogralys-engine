case Element_Kind (Element) is
    when A_Clause =>
        case Clause_Kind (Element) is
            when A_Component_Clause =>
                --  TODO
            when A_Representation_Clause =>
                --  TODO
                case Representation_Clause_Kind (Element) is
                    when A_Record_Representation_Clause =>
                        --  TODO
                    when An_At_Clause =>
                        --  TODO
                    when An_Attribute_Definition_Clause =>
                        --  TODO
                    when An_Enumeration_Representation_Clause =>
                        --  TODO
                    when Not_A_Representation_Clause =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Use_All_Type_Clause =>
                --  TODO
            when A_Use_Package_Clause =>
                --  TODO
            when A_Use_Type_Clause =>
                --  TODO
            when A_With_Clause =>
                --  TODO
            when Not_A_Clause =>
                --  Nothing to do here
                null;
        end case;
    when A_Declaration =>
        case Declaration_Kind (Element) is
            when A_Choice_Parameter_Specification =>
                --  TODO
            when A_Component_Declaration =>
                --  TODO
            when A_Constant_Declaration =>
                --  TODO
            when A_Deferred_Constant_Declaration =>
                --  TODO
            when A_Discriminant_Specification =>
                --  TODO
            when A_Formal_Function_Declaration =>
                --  TODO
                case Default_Kind (Element) is
                    when A_Box_Default =>
                        --  TODO
                    when A_Name_Default =>
                        --  TODO
                    when A_Nil_Default =>
                        --  TODO
                    when A_Null_Default =>
                        --  TODO
                    when Not_A_Default =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Formal_Incomplete_Type_Declaration =>
                --  TODO
            when A_Formal_Object_Declaration =>
                --  TODO
                case Mode_Kind (Element) is
                    when A_Default_In_Mode =>
                        --  TODO
                    when An_In_Mode =>
                        --  TODO
                    when An_In_Out_Mode =>
                        --  TODO
                    when An_Out_Mode =>
                        --  TODO
                    when Not_A_Mode =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Formal_Package_Declaration =>
                --  TODO
            when A_Formal_Package_Declaration_With_Box =>
                --  TODO
            when A_Formal_Procedure_Declaration =>
                --  TODO
                case Default_Kind (Element) is
                    when A_Box_Default =>
                        --  TODO
                    when A_Name_Default =>
                        --  TODO
                    when A_Nil_Default =>
                        --  TODO
                    when A_Null_Default =>
                        --  TODO
                    when Not_A_Default =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Formal_Type_Declaration =>
                --  TODO
            when A_Function_Body_Declaration =>
                --  TODO
            when A_Function_Body_Stub =>
                --  TODO
            when A_Function_Declaration =>
                --  TODO
            when A_Function_Instantiation =>
                --  TODO
            when A_Function_Renaming_Declaration =>
                --  TODO
            when A_Generalized_Iterator_Specification =>
                --  TODO
            when A_Generic_Function_Declaration =>
                --  TODO
            when A_Generic_Function_Renaming_Declaration =>
                --  TODO
            when A_Generic_Package_Declaration =>
                --  TODO
            when A_Generic_Package_Renaming_Declaration =>
                --  TODO
            when A_Generic_Procedure_Declaration =>
                --  TODO
            when A_Generic_Procedure_Renaming_Declaration =>
                --  TODO
            when A_Loop_Parameter_Specification =>
                --  TODO
            when A_Null_Procedure_Declaration =>
                --  TODO
            when A_Package_Body_Declaration =>
                --  TODO
            when A_Package_Body_Stub =>
                --  TODO
            when A_Package_Declaration =>
                --  TODO
            when A_Package_Instantiation =>
                --  TODO
            when A_Package_Renaming_Declaration =>
                --  TODO
            when A_Parameter_Specification =>
                --  TODO
                case Mode_Kind (Element) is
                    when A_Default_In_Mode =>
                        --  TODO
                    when An_In_Mode =>
                        --  TODO
                    when An_In_Out_Mode =>
                        --  TODO
                    when An_Out_Mode =>
                        --  TODO
                    when Not_A_Mode =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Private_Extension_Declaration =>
                --  TODO
            when A_Private_Type_Declaration =>
                --  TODO
            when A_Procedure_Body_Declaration =>
                --  TODO
            when A_Procedure_Body_Stub =>
                --  TODO
            when A_Procedure_Declaration =>
                --  TODO
            when A_Procedure_Instantiation =>
                --  TODO
            when A_Procedure_Renaming_Declaration =>
                --  TODO
            when A_Protected_Body_Declaration =>
                --  TODO
            when A_Protected_Body_Stub =>
                --  TODO
            when A_Protected_Type_Declaration =>
                --  TODO
            when A_Real_Number_Declaration =>
                --  TODO
            when A_Return_Constant_Specification =>
                --  TODO
            when A_Return_Variable_Specification =>
                --  TODO
            when A_Single_Protected_Declaration =>
                --  TODO
            when A_Single_Task_Declaration =>
                --  TODO
            when A_Subtype_Declaration =>
                --  TODO
            when A_Tagged_Incomplete_Type_Declaration =>
                --  TODO
            when A_Task_Body_Declaration =>
                --  TODO
            when A_Task_Body_Stub =>
                --  TODO
            when A_Task_Type_Declaration =>
                --  TODO
            when A_Variable_Declaration =>
                --  TODO
            when An_Element_Iterator_Specification =>
                --  TODO
            when An_Entry_Body_Declaration =>
                --  TODO
            when An_Entry_Declaration =>
                --  TODO
            when An_Entry_Index_Specification =>
                --  TODO
            when An_Enumeration_Literal_Specification =>
                --  TODO
            when An_Exception_Declaration =>
                --  TODO
            when An_Exception_Renaming_Declaration =>
                --  TODO
            when An_Expression_Function_Declaration =>
                --  TODO
            when An_Incomplete_Type_Declaration =>
                --  TODO
            when An_Integer_Number_Declaration =>
                --  TODO
            when An_Object_Renaming_Declaration =>
                --  TODO
            when An_Ordinary_Type_Declaration =>
                --  TODO
            when Not_A_Declaration =>
                --  Nothing to do here
                null;
        end case;
    when A_Defining_Name =>
        case Defining_Name_Kind (Element) is
            when A_Defining_Character_Literal =>
                --  TODO
            when A_Defining_Enumeration_Literal =>
                --  TODO
            when A_Defining_Expanded_Name =>
                --  TODO
            when A_Defining_Identifier =>
                --  TODO
            when A_Defining_Operator_Symbol =>
                --  TODO
                case Operator_Kind (Element) is
                    when A_Concatenate_Operator =>
                        --  TODO
                    when A_Divide_Operator =>
                        --  TODO
                    when A_Greater_Than_Operator =>
                        --  TODO
                    when A_Greater_Than_Or_Equal_Operator =>
                        --  TODO
                    when A_Less_Than_Operator =>
                        --  TODO
                    when A_Less_Than_Or_Equal_Operator =>
                        --  TODO
                    when A_Minus_Operator =>
                        --  TODO
                    when A_Mod_Operator =>
                        --  TODO
                    when A_Multiply_Operator =>
                        --  TODO
                    when A_Not_Equal_Operator =>
                        --  TODO
                    when A_Not_Operator =>
                        --  TODO
                    when A_Plus_Operator =>
                        --  TODO
                    when A_Rem_Operator =>
                        --  TODO
                    when A_Unary_Minus_Operator =>
                        --  TODO
                    when A_Unary_Plus_Operator =>
                        --  TODO
                    when An_Abs_Operator =>
                        --  TODO
                    when An_And_Operator =>
                        --  TODO
                    when An_Equal_Operator =>
                        --  TODO
                    when An_Exponentiate_Operator =>
                        --  TODO
                    when An_Or_Operator =>
                        --  TODO
                    when An_Xor_Operator =>
                        --  TODO
                    when Not_An_Operator =>
                        --  Nothing to do here
                        null;
                end case;
            when Not_A_Defining_Name =>
                --  Nothing to do here
                null;
        end case;
    when A_Definition =>
        case Definition_Kind (Element) is
            when A_Component_Definition =>
                --  TODO
            when A_Constraint =>
                --  TODO
                case Constraint_Kind (Element) is
                    when A_Delta_Constraint =>
                        --  TODO
                    when A_Digits_Constraint =>
                        --  TODO
                    when A_Discriminant_Constraint =>
                        --  TODO
                    when A_Range_Attribute_Reference =>
                        --  TODO
                    when A_Simple_Expression_Range =>
                        --  TODO
                    when An_Index_Constraint =>
                        --  TODO
                    when Not_A_Constraint =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Discrete_Range =>
                --  TODO
                case Discrete_Range_Kind (Element) is
                    when A_Discrete_Range_Attribute_Reference =>
                        --  TODO
                    when A_Discrete_Simple_Expression_Range =>
                        --  TODO
                    when A_Discrete_Subtype_Indication =>
                        --  TODO
                    when Not_A_Discrete_Range =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Discrete_Subtype_Definition =>
                --  TODO
                case Discrete_Range_Kind (Element) is
                    when A_Discrete_Range_Attribute_Reference =>
                        --  TODO
                    when A_Discrete_Simple_Expression_Range =>
                        --  TODO
                    when A_Discrete_Subtype_Indication =>
                        --  TODO
                    when Not_A_Discrete_Range =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Formal_Type_Definition =>
                --  TODO
                case Formal_Type_Kind (Element) is
                    when A_Formal_Access_Type_Definition =>
                        --  TODO
                    when A_Formal_Constrained_Array_Definition =>
                        --  TODO
                    when A_Formal_Decimal_Fixed_Point_Definition =>
                        --  TODO
                    when A_Formal_Derived_Type_Definition =>
                        --  TODO
                    when A_Formal_Discrete_Type_Definition =>
                        --  TODO
                    when A_Formal_Floating_Point_Definition =>
                        --  TODO
                    when A_Formal_Interface_Type_Definition =>
                        --  TODO
                        case Interface_Kind (Element) is
                            when A_Limited_Interface =>
                                --  TODO
                            when A_Protected_Interface =>
                                --  TODO
                            when A_Synchronized_Interface =>
                                --  TODO
                            when A_Task_Interface =>
                                --  TODO
                            when An_Ordinary_Interface =>
                                --  TODO
                            when Not_An_Interface =>
                                --  Nothing to do here
                                null;
                        end case;
                    when A_Formal_Modular_Type_Definition =>
                        --  TODO
                    when A_Formal_Ordinary_Fixed_Point_Definition =>
                        --  TODO
                    when A_Formal_Private_Type_Definition =>
                        --  TODO
                    when A_Formal_Signed_Integer_Type_Definition =>
                        --  TODO
                    when A_Formal_Tagged_Private_Type_Definition =>
                        --  TODO
                    when A_Formal_Unconstrained_Array_Definition =>
                        --  TODO
                    when Not_A_Formal_Type_Definition =>
                        --  Nothing to do here
                        null;
                end case;

                case Interface_Kind (Element) is
                    when A_Limited_Interface =>
                        --  TODO
                    when A_Protected_Interface =>
                        --  TODO
                    when A_Synchronized_Interface =>
                        --  TODO
                    when A_Task_Interface =>
                        --  TODO
                    when An_Ordinary_Interface =>
                        --  TODO
                    when Not_An_Interface =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Known_Discriminant_Part =>
                --  TODO
            when A_Null_Component =>
                --  TODO
            when A_Null_Record_Definition =>
                --  TODO
            when A_Private_Extension_Definition =>
                --  TODO
            when A_Private_Type_Definition =>
                --  TODO
            when A_Protected_Definition =>
                --  TODO
            when A_Record_Definition =>
                --  TODO
            when A_Subtype_Indication =>
                --  TODO
            when A_Tagged_Private_Type_Definition =>
                --  TODO
            when A_Task_Definition =>
                --  TODO
            when A_Type_Definition =>
                --  TODO
                case Type_Kind (Element) is
                    when A_Constrained_Array_Definition =>
                        --  TODO
                    when A_Decimal_Fixed_Point_Definition =>
                        --  TODO
                    when A_Derived_Record_Extension_Definition =>
                        --  TODO
                    when A_Derived_Type_Definition =>
                        --  TODO
                    when A_Floating_Point_Definition =>
                        --  TODO
                    when A_Modular_Type_Definition =>
                        --  TODO
                    when A_Record_Type_Definition =>
                        --  TODO
                    when A_Root_Type_Definition =>
                        --  TODO
                        case Root_Type_Kind (Element) is
                            when A_Root_Integer_Definition =>
                                --  TODO
                            when A_Root_Real_Definition =>
                                --  TODO
                            when A_Universal_Fixed_Definition =>
                                --  TODO
                            when A_Universal_Integer_Definition =>
                                --  TODO
                            when A_Universal_Real_Definition =>
                                --  TODO
                            when Not_A_Root_Type_Definition =>
                                --  Nothing to do here
                                null;
                        end case;
                    when A_Signed_Integer_Type_Definition =>
                        --  TODO
                    when A_Tagged_Record_Type_Definition =>
                        --  TODO
                    when An_Access_Type_Definition =>
                        --  TODO
                        case Access_Type_Kind (Element) is
                            when A_Pool_Specific_Access_To_Variable =>
                                --  TODO
                            when An_Access_To_Constant =>
                                --  TODO
                            when An_Access_To_Function =>
                                --  TODO
                            when An_Access_To_Procedure =>
                                --  TODO
                            when An_Access_To_Protected_Function =>
                                --  TODO
                            when An_Access_To_Protected_Procedure =>
                                --  TODO
                            when An_Access_To_Variable =>
                                --  TODO
                            when Not_An_Access_Type_Definition =>
                                --  Nothing to do here
                                null;
                        end case;
                    when An_Enumeration_Type_Definition =>
                        --  TODO
                    when An_Interface_Type_Definition =>
                        --  TODO
                        case Interface_Kind (Element) is
                            when A_Limited_Interface =>
                                --  TODO
                            when A_Protected_Interface =>
                                --  TODO
                            when A_Synchronized_Interface =>
                                --  TODO
                            when A_Task_Interface =>
                                --  TODO
                            when An_Ordinary_Interface =>
                                --  TODO
                            when Not_An_Interface =>
                                --  Nothing to do here
                                null;
                        end case;
                    when An_Ordinary_Fixed_Point_Definition =>
                        --  TODO
                    when An_Unconstrained_Array_Definition =>
                        --  TODO
                    when Not_A_Type_Definition =>
                        --  Nothing to do here
                        null;
                end case;

                case Interface_Kind (Element) is
                    when A_Limited_Interface =>
                        --  TODO
                    when A_Protected_Interface =>
                        --  TODO
                    when A_Synchronized_Interface =>
                        --  TODO
                    when A_Task_Interface =>
                        --  TODO
                    when An_Ordinary_Interface =>
                        --  TODO
                    when Not_An_Interface =>
                        --  Nothing to do here
                        null;
                end case;
            when A_Variant =>
                --  TODO
            when A_Variant_Part =>
                --  TODO
            when An_Access_Definition =>
                --  TODO
                case Access_Definition_Kind (Element) is
                    when An_Anonymous_Access_To_Constant =>
                        --  TODO
                    when An_Anonymous_Access_To_Function =>
                        --  TODO
                    when An_Anonymous_Access_To_Procedure =>
                        --  TODO
                    when An_Anonymous_Access_To_Protected_Function =>
                        --  TODO
                    when An_Anonymous_Access_To_Protected_Procedure =>
                        --  TODO
                    when An_Anonymous_Access_To_Variable =>
                        --  TODO
                    when Not_An_Access_Definition =>
                        --  Nothing to do here
                        null;
                end case;
            when An_Aspect_Specification =>
                --  TODO
            when An_Others_Choice =>
                --  TODO
            when An_Unknown_Discriminant_Part =>
                --  TODO
            when Not_A_Definition =>
                --  Nothing to do here
                null;
        end case;
    when A_Path =>
        case Path_Kind (Element) is
            when A_Case_Expression_Path =>
                --  TODO
            when A_Case_Path =>
                --  TODO
            when A_Select_Path =>
                --  TODO
            when A_Then_Abort_Path =>
                --  TODO
            when An_Else_Expression_Path =>
                --  TODO
            when An_Else_Path =>
                --  TODO
            when An_Elsif_Expression_Path =>
                --  TODO
            when An_Elsif_Path =>
                --  TODO
            when An_If_Expression_Path =>
                --  TODO
            when An_If_Path =>
                --  TODO
            when An_Or_Path =>
                --  TODO
            when Not_A_Path =>
                --  Nothing to do here
                null;
        end case;
    when A_Pragma =>
        case Pragma_Kind (Element) is
            when A_Controlled_Pragma =>
                --  TODO
            when A_Convention_Pragma =>
                --  TODO
            when A_CPU_Pragma =>
                --  TODO
            when A_Default_Storage_Pool_Pragma =>
                --  TODO
            when A_Detect_Blocking_Pragma =>
                --  TODO
            when A_Discard_Names_Pragma =>
                --  TODO
            when A_Dispatching_Domain_Pragma =>
                --  TODO
            when A_Independent_Components_Pragma =>
                --  TODO
            when A_Linker_Options_Pragma =>
                --  TODO
            when A_List_Pragma =>
                --  TODO
            when A_Locking_Policy_Pragma =>
                --  TODO
            when A_No_Return_Pragma =>
                --  TODO
            when A_Normalize_Scalars_Pragma =>
                --  TODO
            when A_Pack_Pragma =>
                --  TODO
            when A_Page_Pragma =>
                --  TODO
            when A_Partition_Elaboration_Policy_Pragma =>
                --  TODO
            when A_Preelaborable_Initialization_Pragma =>
                --  TODO
            when A_Preelaborate_Pragma =>
                --  TODO
            when A_Priority_Pragma =>
                --  TODO
            when A_Priority_Specific_Dispatching_Pragma =>
                --  TODO
            when A_Profile_Pragma =>
                --  TODO
            when A_Pure_Pragma =>
                --  TODO
            when A_Queuing_Policy_Pragma =>
                --  TODO
            when A_Relative_Deadline_Pragma =>
                --  TODO
            when A_Remote_Call_Interface_Pragma =>
                --  TODO
            when A_Remote_Types_Pragma =>
                --  TODO
            when A_Restrictions_Pragma =>
                --  TODO
            when A_Reviewable_Pragma =>
                --  TODO
            when A_Shared_Passive_Pragma =>
                --  TODO
            when A_Storage_Size_Pragma =>
                --  TODO
            when A_Suppress_Pragma =>
                --  TODO
            when A_Task_Dispatching_Policy_Pragma =>
                --  TODO
            when A_Volatile_Components_Pragma =>
                --  TODO
            when A_Volatile_Pragma =>
                --  TODO
            when An_All_Calls_Remote_Pragma =>
                --  TODO
            when An_Assert_Pragma =>
                --  TODO
            when An_Assertion_Policy_Pragma =>
                --  TODO
            when An_Asynchronous_Pragma =>
                --  TODO
            when An_Atomic_Components_Pragma =>
                --  TODO
            when An_Atomic_Pragma =>
                --  TODO
            when An_Attach_Handler_Pragma =>
                --  TODO
            when An_Elaborate_All_Pragma =>
                --  TODO
            when An_Elaborate_Body_Pragma =>
                --  TODO
            when An_Elaborate_Pragma =>
                --  TODO
            when An_Export_Pragma =>
                --  TODO
            when An_Implementation_Defined_Pragma =>
                --  TODO
            when An_Import_Pragma =>
                --  TODO
            when An_Independent_Pragma =>
                --  TODO
            when An_Inline_Pragma =>
                --  TODO
            when An_Inspection_Point_Pragma =>
                --  TODO
            when An_Interrupt_Handler_Pragma =>
                --  TODO
            when An_Interrupt_Priority_Pragma =>
                --  TODO
            when An_Optimize_Pragma =>
                --  TODO
            when An_Unchecked_Union_Pragma =>
                --  TODO
            when An_Unknown_Pragma =>
                --  TODO
            when An_Unsuppress_Pragma =>
                --  TODO
            when Not_A_Pragma =>
                --  Nothing to do here
                null;
        end case;
    when A_Statement =>
        case Statement_Kind (Element) is
            when A_Block_Statement =>
                --  TODO
            when A_Case_Statement =>
                --  TODO
            when A_Code_Statement =>
                --  TODO
            when A_Conditional_Entry_Call_Statement =>
                --  TODO
            when A_Delay_Relative_Statement =>
                --  TODO
            when A_Delay_Until_Statement =>
                --  TODO
            when A_For_Loop_Statement =>
                --  TODO
            when A_Goto_Statement =>
                --  TODO
            when A_Loop_Statement =>
                --  TODO
            when A_Null_Statement =>
                --  TODO
            when A_Procedure_Call_Statement =>
                --  TODO
            when A_Raise_Statement =>
                --  TODO
            when A_Requeue_Statement =>
                --  TODO
            when A_Requeue_Statement_With_Abort =>
                --  TODO
            when A_Return_Statement =>
                --  TODO
            when A_Selective_Accept_Statement =>
                --  TODO
            when A_Terminate_Alternative_Statement =>
                --  TODO
            when A_Timed_Entry_Call_Statement =>
                --  TODO
            when A_While_Loop_Statement =>
                --  TODO
            when An_Abort_Statement =>
                --  TODO
            when An_Accept_Statement =>
                --  TODO
            when An_Assignment_Statement =>
                --  TODO
            when An_Asynchronous_Select_Statement =>
                --  TODO
            when An_Entry_Call_Statement =>
                --  TODO
            when An_Exit_Statement =>
                --  TODO
            when An_Extended_Return_Statement =>
                --  TODO
            when An_If_Statement =>
                --  TODO
            when Not_A_Statement =>
                --  Nothing to do here
                null;
        end case;
    when An_Association =>
        case Association_Kind (Element) is
            when A_Discriminant_Association =>
                --  TODO
            when A_Generic_Association =>
                --  TODO
            when A_Parameter_Association =>
                --  TODO
            when A_Pragma_Argument_Association =>
                --  TODO
            when A_Record_Component_Association =>
                --  TODO
            when An_Array_Component_Association =>
                --  TODO
            when Not_An_Association =>
                --  Nothing to do here
                null;
        end case;
    when An_Exception_Handler =>
        --  There is no "Exception_Handler_Kind".
        --  TODO
        null;
    when An_Expression =>
        case Expression_Kind (Element) is
            when A_Box_Expression =>
                --  TODO
            when A_Case_Expression =>
                --  TODO
            when A_Character_Literal =>
                --  TODO
            when A_For_All_Quantified_Expression =>
                --  TODO
            when A_For_Some_Quantified_Expression =>
                --  TODO
            when A_Function_Call =>
                --  TODO
            when A_Named_Array_Aggregate =>
                --  TODO
            when A_Not_In_Membership_Test =>
                --  TODO
            when A_Null_Literal =>
                --  TODO
            when A_Parenthesized_Expression =>
                --  TODO
            when A_Positional_Array_Aggregate =>
                --  TODO
            when A_Qualified_Expression =>
                --  TODO
            when A_Raise_Expression =>
                --  TODO
            when A_Real_Literal =>
                --  TODO
            when A_Record_Aggregate =>
                --  TODO
            when A_Selected_Component =>
                --  TODO
            when A_Slice =>
                --  TODO
            when A_String_Literal =>
                --  TODO
            when A_Type_Conversion =>
                --  TODO
            when An_Allocation_From_Qualified_Expression =>
                --  TODO
            when An_Allocation_From_Subtype =>
                --  TODO
            when An_And_Then_Short_Circuit =>
                --  TODO
            when An_Attribute_Reference =>
                --  TODO
                case Attribute_Kind (Element) is
                    when A_Base_Attribute =>
                        --  TODO
                    when A_Bit_Order_Attribute =>
                        --  TODO
                    when A_Body_Version_Attribute =>
                        --  TODO
                    when A_Callable_Attribute =>
                        --  TODO
                    when A_Caller_Attribute =>
                        --  TODO
                    when A_Ceiling_Attribute =>
                        --  TODO
                    when A_Class_Attribute =>
                        --  TODO
                    when A_Component_Size_Attribute =>
                        --  TODO
                    when A_Compose_Attribute =>
                        --  TODO
                    when A_Constrained_Attribute =>
                        --  TODO
                    when A_Copy_Sign_Attribute =>
                        --  TODO
                    when A_Count_Attribute =>
                        --  TODO
                    when A_Definite_Attribute =>
                        --  TODO
                    when A_Delta_Attribute =>
                        --  TODO
                    when A_Denorm_Attribute =>
                        --  TODO
                    when A_Digits_Attribute =>
                        --  TODO
                    when A_First_Attribute =>
                        --  TODO
                    when A_First_Bit_Attribute =>
                        --  TODO
                    when A_Floor_Attribute =>
                        --  TODO
                    when A_Fore_Attribute =>
                        --  TODO
                    when A_Fraction_Attribute =>
                        --  TODO
                    when A_Last_Attribute =>
                        --  TODO
                    when A_Last_Bit_Attribute =>
                        --  TODO
                    when A_Leading_Part_Attribute =>
                        --  TODO
                    when A_Length_Attribute =>
                        --  TODO
                    when A_Machine_Attribute =>
                        --  TODO
                    when A_Machine_Emax_Attribute =>
                        --  TODO
                    when A_Machine_Emin_Attribute =>
                        --  TODO
                    when A_Machine_Mantissa_Attribute =>
                        --  TODO
                    when A_Machine_Overflows_Attribute =>
                        --  TODO
                    when A_Machine_Radix_Attribute =>
                        --  TODO
                    when A_Machine_Rounding_Attribute =>
                        --  TODO
                    when A_Machine_Rounds_Attribute =>
                        --  TODO
                    when A_Max_Alignment_For_Allocation_Attribute =>
                        --  TODO
                    when A_Max_Attribute =>
                        --  TODO
                    when A_Max_Size_In_Storage_Elements_Attribute =>
                        --  TODO
                    when A_Min_Attribute =>
                        --  TODO
                    when A_Mod_Attribute =>
                        --  TODO
                    when A_Model_Attribute =>
                        --  TODO
                    when A_Model_Emin_Attribute =>
                        --  TODO
                    when A_Model_Epsilon_Attribute =>
                        --  TODO
                    when A_Model_Mantissa_Attribute =>
                        --  TODO
                    when A_Model_Small_Attribute =>
                        --  TODO
                    when A_Modulus_Attribute =>
                        --  TODO
                    when A_Partition_ID_Attribute =>
                        --  TODO
                    when A_Pos_Attribute =>
                        --  TODO
                    when A_Position_Attribute =>
                        --  TODO
                    when A_Pred_Attribute =>
                        --  TODO
                    when A_Priority_Attribute =>
                        --  TODO
                    when A_Range_Attribute =>
                        --  TODO
                    when A_Read_Attribute =>
                        --  TODO
                    when A_Remainder_Attribute =>
                        --  TODO
                    when A_Round_Attribute =>
                        --  TODO
                    when A_Rounding_Attribute =>
                        --  TODO
                    when A_Safe_First_Attribute =>
                        --  TODO
                    when A_Safe_Last_Attribute =>
                        --  TODO
                    when A_Scale_Attribute =>
                        --  TODO
                    when A_Scaling_Attribute =>
                        --  TODO
                    when A_Signed_Zeros_Attribute =>
                        --  TODO
                    when A_Size_Attribute =>
                        --  TODO
                    when A_Small_Attribute =>
                        --  TODO
                    when A_Storage_Pool_Attribute =>
                        --  TODO
                    when A_Storage_Size_Attribute =>
                        --  TODO
                    when A_Stream_Size_Attribute =>
                        --  TODO
                    when A_Succ_Attribute =>
                        --  TODO
                    when A_Tag_Attribute =>
                        --  TODO
                    when A_Terminated_Attribute =>
                        --  TODO
                    when A_Truncation_Attribute =>
                        --  TODO
                    when A_Val_Attribute =>
                        --  TODO
                    when A_Valid_Attribute =>
                        --  TODO
                    when A_Value_Attribute =>
                        --  TODO
                    when A_Version_Attribute =>
                        --  TODO
                    when A_Wide_Image_Attribute =>
                        --  TODO
                    when A_Wide_Value_Attribute =>
                        --  TODO
                    when A_Wide_Wide_Image_Attribute =>
                        --  TODO
                    when A_Wide_Wide_Value_Attribute =>
                        --  TODO
                    when A_Wide_Wide_Width_Attribute =>
                        --  TODO
                    when A_Wide_Width_Attribute =>
                        --  TODO
                    when A_Width_Attribute =>
                        --  TODO
                    when A_Write_Attribute =>
                        --  TODO
                    when An_Access_Attribute =>
                        --  TODO
                    when An_Address_Attribute =>
                        --  TODO
                    when An_Adjacent_Attribute =>
                        --  TODO
                    when An_Aft_Attribute =>
                        --  TODO
                    when An_Alignment_Attribute =>
                        --  TODO
                    when An_Exponent_Attribute =>
                        --  TODO
                    when An_External_Tag_Attribute =>
                        --  TODO
                    when An_Identity_Attribute =>
                        --  TODO
                    when An_Image_Attribute =>
                        --  TODO
                    when An_Implementation_Defined_Attribute =>
                        --  TODO
                    when An_Input_Attribute =>
                        --  TODO
                    when An_Output_Attribute =>
                        --  TODO
                    when An_Overlaps_Storage_Attribute =>
                        --  TODO
                    when An_Unbiased_Rounding_Attribute =>
                        --  TODO
                    when An_Unchecked_Access_Attribute =>
                        --  TODO
                    when An_Unknown_Attribute =>
                        --  TODO
                    when Not_An_Attribute =>
                        --  Nothing to do here
                        null;
                end case;
            when An_Enumeration_Literal =>
                --  TODO
            when An_Explicit_Dereference =>
                --  TODO
            when An_Extension_Aggregate =>
                --  TODO
            when An_Identifier =>
                --  TODO
            when An_If_Expression =>
                --  TODO
            when An_In_Membership_Test =>
                --  TODO
            when An_Indexed_Component =>
                --  TODO
            when An_Integer_Literal =>
                --  TODO
            when An_Operator_Symbol =>
                --  TODO
                case Operator_Kind (Element) is
                    when A_Concatenate_Operator =>
                        --  TODO
                    when A_Divide_Operator =>
                        --  TODO
                    when A_Greater_Than_Operator =>
                        --  TODO
                    when A_Greater_Than_Or_Equal_Operator =>
                        --  TODO
                    when A_Less_Than_Operator =>
                        --  TODO
                    when A_Less_Than_Or_Equal_Operator =>
                        --  TODO
                    when A_Minus_Operator =>
                        --  TODO
                    when A_Mod_Operator =>
                        --  TODO
                    when A_Multiply_Operator =>
                        --  TODO
                    when A_Not_Equal_Operator =>
                        --  TODO
                    when A_Not_Operator =>
                        --  TODO
                    when A_Plus_Operator =>
                        --  TODO
                    when A_Rem_Operator =>
                        --  TODO
                    when A_Unary_Minus_Operator =>
                        --  TODO
                    when A_Unary_Plus_Operator =>
                        --  TODO
                    when An_Abs_Operator =>
                        --  TODO
                    when An_And_Operator =>
                        --  TODO
                    when An_Equal_Operator =>
                        --  TODO
                    when An_Exponentiate_Operator =>
                        --  TODO
                    when An_Or_Operator =>
                        --  TODO
                    when An_Xor_Operator =>
                        --  TODO
                    when Not_An_Operator =>
                        --  Nothing to do here
                        null;
                end case;
            when An_Or_Else_Short_Circuit =>
                --  TODO
            when Not_An_Expression =>
                --  Nothing to do here
                null;
        end case;
    when Not_An_Element =>
        --  Nothing to do here
        null;
end case;
