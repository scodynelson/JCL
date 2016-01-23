/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.struct.ValuesStructs;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.structures.StructureClassStruct;
import jcl.structures.StructureObjectStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TypeBaseClass;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.objectweb.asm.Type;

interface GenerationConstants {

	String JAVA_EXTENSION = ".java";

	String SINGLETON_INSTANCE = "INSTANCE";

	String INIT_METHOD_NAME = "<init>";

	String INIT_METHOD_DESC = "()V";

	String CLASS_INIT_METHOD_NAME = "<clinit>";

	String CLASS_INIT_METHOD_DESC = "()V";

	String JAVA_OBJECT_NAME = Type.getInternalName(Object.class);

	String JAVA_EQUALS_METHOD_NAME = "equals";

	String JAVA_EQUALS_METHOD_DESC = CodeGenerators.getMethodDescription(Object.class, JAVA_EQUALS_METHOD_NAME, Object.class);

	String JAVA_HASH_CODE_METHOD_NAME = "hashCode";

	String JAVA_HASH_CODE_METHOD_DESC = CodeGenerators.getMethodDescription(Object.class, JAVA_HASH_CODE_METHOD_NAME);

	String JAVA_LIST_NAME = Type.getInternalName(List.class);

	String JAVA_LIST_ADD_METHOD_NAME = "add";

	String JAVA_LIST_ADD_METHOD_DESC = CodeGenerators.getMethodDescription(List.class, JAVA_LIST_ADD_METHOD_NAME, Object.class);

	String JAVA_LIST_GET_METHOD_NAME = "get";

	String JAVA_LIST_GET_METHOD_DESC = CodeGenerators.getMethodDescription(List.class, JAVA_LIST_GET_METHOD_NAME, int.class);

	String JAVA_LIST_SIZE_METHOD_NAME = "size";

	String JAVA_LIST_SIZE_METHOD_DESC = CodeGenerators.getMethodDescription(List.class, JAVA_LIST_SIZE_METHOD_NAME);

	String JAVA_LIST_ITERATOR_METHOD_NAME = "iterator";

	String JAVA_LIST_ITERATOR_METHOD_DESC = CodeGenerators.getMethodDescription(List.class, JAVA_LIST_ITERATOR_METHOD_NAME);

	String JAVA_LIST_TO_ARRAY_METHOD_NAME = "toArray";

	String JAVA_LIST_TO_ARRAY_METHOD_DESC = CodeGenerators.getMethodDescription(List.class, JAVA_LIST_TO_ARRAY_METHOD_NAME, Object[].class);

	String JAVA_ARRAY_LIST_NAME = Type.getInternalName(ArrayList.class);

	String JAVA_ARRAY_LIST_INIT_DESC = CodeGenerators.getConstructorDescription(ArrayList.class);

	String JAVA_MAP_NAME = Type.getInternalName(Map.class);

	String JAVA_MAP_DESC = Type.getDescriptor(Map.class);

	String JAVA_MAP_PUT_METHOD_NAME = "put";

	String JAVA_MAP_PUT_METHOD_DESC = CodeGenerators.getMethodDescription(Map.class, JAVA_MAP_PUT_METHOD_NAME, Object.class, Object.class);

	String JAVA_ITERATOR_NAME = Type.getInternalName(Iterator.class);

	String JAVA_ITERATOR_HAS_NEXT_METHOD_NAME = "hasNext";

	String JAVA_ITERATOR_HAS_NEXT_METHOD_DESC = CodeGenerators.getMethodDescription(Iterator.class, JAVA_ITERATOR_HAS_NEXT_METHOD_NAME);

	String JAVA_ITERATOR_NEXT_METHOD_NAME = "next";

	String JAVA_ITERATOR_NEXT_METHOD_DESC = CodeGenerators.getMethodDescription(Iterator.class, JAVA_ITERATOR_NEXT_METHOD_NAME);

	String JAVA_INTEGER_NAME = Type.getInternalName(Integer.class);

	String JAVA_INTEGER_VALUE_OF_METHOD_NAME = "valueOf";

	String JAVA_INTEGER_VALUE_OF_METHOD_DESC = CodeGenerators.getMethodDescription(Integer.class, JAVA_INTEGER_VALUE_OF_METHOD_NAME, int.class);

	String JAVA_BIG_INTEGER_NAME = Type.getInternalName(BigInteger.class);

	String JAVA_BIG_INTEGER_INIT_DESC = CodeGenerators.getConstructorDescription(BigInteger.class, String.class);

	String JAVA_BIG_DECIMAL_NAME = Type.getInternalName(BigDecimal.class);

	String JAVA_BIG_DECIMAL_INIT_DESC = CodeGenerators.getConstructorDescription(BigDecimal.class, String.class);

	String JAVA_URI_NAME = Type.getInternalName(URI.class);

	String JAVA_URI_CREATE_METHOD_NAME = "create";

	String JAVA_URI_CREATE_METHOD_DESC = CodeGenerators.getMethodDescription(URI.class, JAVA_URI_CREATE_METHOD_NAME, String.class);

	String JAVA_STRING_BUILDER_NAME = Type.getInternalName(StringBuilder.class);

	String JAVA_STRING_BUILDER_INIT_DESC = CodeGenerators.getConstructorDescription(StringBuilder.class);

	String JAVA_STRING_BUILDER_APPEND_METHOD_NAME = "append";

	String JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC = CodeGenerators.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_APPEND_METHOD_NAME, String.class);

	String JAVA_STRING_BUILDER_APPEND_OBJECT_METHOD_DESC = CodeGenerators.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_APPEND_METHOD_NAME, Object.class);

	String JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME = "toString";

	String JAVA_STRING_BUILDER_TO_STRING_METHOD_DESC = CodeGenerators.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME);

	String HASH_CODE_BUILDER_NAME = "org/apache/commons/lang3/builder/HashCodeBuilder";

	String HASH_CODE_BUILDER_APPEND_SUPER_METHOD_NAME = "appendSuper";

	String HASH_CODE_BUILDER_APPEND_SUPER_METHOD_DESC = CodeGenerators.getMethodDescription(HashCodeBuilder.class, HASH_CODE_BUILDER_APPEND_SUPER_METHOD_NAME, int.class);

	String HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_NAME = "toHashCode";

	String HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_DESC = CodeGenerators.getMethodDescription(HashCodeBuilder.class, HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_NAME);

	String SYMBOL_STRUCT_NAME = Type.getInternalName(SymbolStruct.class);

	String SYMBOL_STRUCT_INIT_STRING_DESC = CodeGenerators.getConstructorDescription(SymbolStruct.class, String.class);

	String SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME = "getLexicalValue";

	String SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_NAME = "setLexicalValue";

	String SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME = "bindLexicalValue";

	String SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME = "unbindLexicalValue";

	String SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME = "getDynamicValue";

	String SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_NAME = "setDynamicValue";

	String SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME = "bindDynamicValue";

	String SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME = "unbindDynamicValue";

	String SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_GET_VALUE_METHOD_NAME = "getValue";

	String SYMBOL_STRUCT_GET_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_SET_VALUE_METHOD_NAME = "setValue";

	String SYMBOL_STRUCT_SET_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME = "getFunction";

	String SYMBOL_STRUCT_GET_FUNCTION_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME);

	String SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME = "bindFunction";

	String SYMBOL_STRUCT_BIND_FUNCTION_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME, FunctionStruct.class);

	String SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME = "unbindFunction";

	String SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME);

	String SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME = "setStructureClass";

	String SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME, StructureClassStruct.class);

	String SYMBOL_STRUCT_SET_MACRO_FUNCTION_EXPANDER_METHOD_NAME = "setMacroFunctionExpander";

	String SYMBOL_STRUCT_SET_MACRO_FUNCTION_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_MACRO_FUNCTION_EXPANDER_METHOD_NAME, MacroFunctionExpander.class);

	String PACKAGE_STRUCT_NAME = Type.getInternalName(PackageStruct.class);

	String PACKAGE_STRUCT_DESC = Type.getDescriptor(PackageStruct.class);

	String PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME = "findPackage";

	String PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC = CodeGenerators.getMethodDescription(PackageStruct.class, PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME, String.class);

	String PACKAGE_STRUCT_INTERN_METHOD_NAME = "intern";

	String PACKAGE_STRUCT_INTERN_METHOD_DESC = CodeGenerators.getMethodDescription(PackageStruct.class, PACKAGE_STRUCT_INTERN_METHOD_NAME, String.class);

	String PACKAGE_SYMBOL_STRUCT_NAME = Type.getInternalName(PackageSymbolStruct.class);

	String PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME = "getSymbol";

	String PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC = CodeGenerators.getMethodDescription(PackageSymbolStruct.class, PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME);

	String GLOBAL_PACKAGE_STRUCT_NAME = Type.getInternalName(GlobalPackageStruct.class);

	String KEYWORD_STRUCT_NAME = Type.getInternalName(KeywordStruct.class);

	String FUNCTION_STRUCT_NAME = Type.getInternalName(FunctionStruct.class);

	String FUNCTION_STRUCT_INIT_DESC = CodeGenerators.getConstructorDescription(FunctionStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link FunctionStruct#FunctionStruct(Closure)}
	 * constructor method.
	 */
	String FUNCTION_STRUCT_INIT_CLOSURE_DESC = CodeGenerators.getConstructorDescription(FunctionStruct.class, Closure.class);

	String FUNCTION_STRUCT_INIT_STRING_CLOSURE_DESC = CodeGenerators.getConstructorDescription(FunctionStruct.class, String.class, Closure.class);

	String FUNCTION_STRUCT_APPLY_METHOD_NAME = "apply";

	String FUNCTION_STRUCT_APPLY_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_APPLY_METHOD_NAME, LispStruct[].class);

	String MACRO_FUNCTION_EXPANDER_NAME = Type.getInternalName(MacroFunctionExpander.class);

	String MACRO_FUNCTION_EXPANDER_CLASS_SIGNATURE = "Ljcl/functions/expanders/MacroFunctionExpander<Ljcl/LispStruct;>;";

	String LISP_STRUCT_NAME = Type.getInternalName(LispStruct.class);

	String LISP_STRUCT_DESC = Type.getDescriptor(LispStruct.class);

	String LISP_STRUCT_ARRAY_DESC = Type.getDescriptor(LispStruct[].class);

	String VALUES_STRUCTS_NAME = Type.getInternalName(ValuesStructs.class);

	String VALUES_STRUCTS_ADD_VALUES_TO_LIST_METHOD_NAME = "addValuesToList";

	String VALUES_STRUCTS_ADD_VALUES_TO_LIST_METHOD_DESC = CodeGenerators.getMethodDescription(ValuesStructs.class, VALUES_STRUCTS_ADD_VALUES_TO_LIST_METHOD_NAME, List.class, LispStruct.class);

	String VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_NAME = "extractPrimaryValue";

	String VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(ValuesStructs.class, VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_NAME, LispStruct.class);

	String CLOSURE_NAME = Type.getInternalName(Closure.class);

	String CLOSURE_INIT_CLOSURE_DESC = CodeGenerators.getConstructorDescription(Closure.class, Closure.class);

	String CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME = "getFunctionBindings";

	String CLOSURE_GET_FUNCTION_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(Closure.class, CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME);

	String CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME = "getSymbolBindings";

	String CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(Closure.class, CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME);

	String ORDINARY_LAMBDA_LIST_BINDINGS_NAME = Type.getInternalName(OrdinaryLambdaList.class);

	String ORDINARY_LAMBDA_LIST_BINDINGS_DESC = Type.getDescriptor(OrdinaryLambdaList.class);

	String ORDINARY_LAMBDA_LIST_BINDINGS_INIT_DESC = CodeGenerators.getConstructorDescription(OrdinaryLambdaList.class, List.class, List.class, RestParameter.class, List.class, List.class, boolean.class);

	String PROGRAM_ERROR_EXCEPTION_NAME = Type.getInternalName(ProgramErrorException.class);

	String PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC = CodeGenerators.getConstructorDescription(ProgramErrorException.class, String.class);

	String REQUIRED_BINDING_NAME = Type.getInternalName(RequiredParameter.class);

	String REQUIRED_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(RequiredParameter.class, SymbolStruct.class, boolean.class);

	String OPTIONAL_BINDING_NAME = Type.getInternalName(OptionalParameter.class);

	String OPTIONAL_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(OptionalParameter.class, SymbolStruct.class, LispStruct.class, boolean.class, SuppliedPParameter.class);

	String REST_BINDING_NAME = Type.getInternalName(RestParameter.class);

	String REST_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(RestParameter.class, SymbolStruct.class, boolean.class);

	String KEY_BINDING_NAME = Type.getInternalName(KeyParameter.class);

	String KEY_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(KeyParameter.class, SymbolStruct.class, LispStruct.class, boolean.class, SymbolStruct.class, SuppliedPParameter.class);

	String AUX_BINDING_NAME = Type.getInternalName(AuxParameter.class);

	String AUX_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(AuxParameter.class, SymbolStruct.class, LispStruct.class, boolean.class);

	String SUPPLIED_P_BINDING_NAME = Type.getInternalName(SuppliedPParameter.class);

	String SUPPLIED_P_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(SuppliedPParameter.class, SymbolStruct.class, boolean.class);

	String WHOLE_BINDING_NAME = Type.getInternalName(WholeParameter.class);

	String WHOLE_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(WholeParameter.class, SymbolStruct.class, boolean.class);

	String ENVIRONMENT_BINDING_NAME = Type.getInternalName(EnvironmentParameter.class);

	String ENVIRONMENT_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(EnvironmentParameter.class, SymbolStruct.class);

	String BODY_BINDING_NAME = Type.getInternalName(BodyParameter.class);

	String BODY_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(BodyParameter.class, SymbolStruct.class, boolean.class);

	String RETURN_FROM_EXCEPTION_NAME = Type.getInternalName(ReturnFromException.class);

	String RETURN_FROM_EXCEPTION_INIT_DESC = CodeGenerators.getConstructorDescription(ReturnFromException.class, SymbolStruct.class, LispStruct.class);

	String RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME = "getName";

	String RETURN_FROM_EXCEPTION_GET_NAME_METHOD_DESC = CodeGenerators.getMethodDescription(ReturnFromException.class, RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME);

	String RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME = "getResult";

	String RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_DESC = CodeGenerators.getMethodDescription(ReturnFromException.class, RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME);

	String GO_EXCEPTION_NAME = Type.getInternalName(GoException.class);

	String GO_EXCEPTION_INIT_DESC = CodeGenerators.getConstructorDescription(GoException.class, int.class);

	String GO_EXCEPTION_GET_TAG_INDEX_METHOD_NAME = "getTagIndex";

	String GO_EXCEPTION_GET_TAG_INDEX_METHOD_DESC = CodeGenerators.getMethodDescription(GoException.class, GO_EXCEPTION_GET_TAG_INDEX_METHOD_NAME);

	String THROW_EXCEPTION_NAME = Type.getInternalName(ThrowException.class);

	String THROW_EXCEPTION_INIT_DESC = CodeGenerators.getConstructorDescription(ThrowException.class, LispStruct.class, LispStruct.class);

	String THROW_EXCEPTION_GET_CATCH_TAG_METHOD_NAME = "getCatchTag";

	String THROW_EXCEPTION_GET_CATCH_TAG_METHOD_DESC = CodeGenerators.getMethodDescription(ThrowException.class, THROW_EXCEPTION_GET_CATCH_TAG_METHOD_NAME);

	String THROW_EXCEPTION_GET_RESULT_FORM_METHOD_NAME = "getResultForm";

	String THROW_EXCEPTION_GET_RESULT_FORM_METHOD_DESC = CodeGenerators.getMethodDescription(ThrowException.class, THROW_EXCEPTION_GET_RESULT_FORM_METHOD_NAME);

	String CONS_STRUCT_NAME = Type.getInternalName(ConsStruct.class);

	String CONS_STRUCT_INIT_CAR_DESC = CodeGenerators.getConstructorDescription(ConsStruct.class, LispStruct.class);

	String CONS_STRUCT_INIT_CAR_CDR_DESC = CodeGenerators.getConstructorDescription(ConsStruct.class, LispStruct.class, LispStruct.class);

	String LIST_STRUCT_NAME = Type.getInternalName(ListStruct.class);

	String LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME = "getAsJavaList";

	String LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_DESC = CodeGenerators.getMethodDescription(ListStruct.class, LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME);

	String STRUCTURE_OBJECT_STRUCT_NAME = Type.getInternalName(StructureObjectStruct.class);

	String TYPE_BASE_CLASS_NAME = Type.getInternalName(TypeBaseClass.class);

	String TYPE_BASE_CLASS_INIT_STRING_DESC = CodeGenerators.getConstructorDescription(TypeBaseClass.class, String.class);

	String ATOMIC_TYPE_SPECIFIER_NAME = Type.getInternalName(AtomicTypeSpecifier.class);

	String TYPE_FACTORY_NAME = Type.getInternalName(TypeFactory.class);
}
