/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.icg.generator.specialoperator.exception.GoException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ReturnFromException;
import jcl.compiler.real.icg.generator.specialoperator.exception.ThrowException;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionParameterBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Type;

public interface GenerationConstants {

	String SINGLETON_INSTANCE = "INSTANCE";

	String INIT_METHOD_NAME = "<init>";

	String CLASS_INIT_METHOD_NAME = "<clinit>";

	String JAVA_OBJECT_NAME = Type.getInternalName(Object.class);

	String JAVA_EQUALS_METHOD_NAME = "equals";

	String JAVA_EQUALS_METHOD_DESC = GeneratorUtils.getMethodDescription(Object.class, JAVA_EQUALS_METHOD_NAME, Object.class);

	String JAVA_STRING_NAME = Type.getInternalName(String.class);

	String JAVA_LIST_NAME = Type.getInternalName(List.class);

	String JAVA_LIST_ADD_METHOD_NAME = "add";

	String JAVA_LIST_ADD_METHOD_DESC = GeneratorUtils.getMethodDescription(List.class, JAVA_LIST_ADD_METHOD_NAME, Object.class);

	String JAVA_ARRAY_LIST_NAME = Type.getInternalName(ArrayList.class);

	String JAVA_ARRAY_LIST_INIT_DESC = GeneratorUtils.getConstructorDescription(ArrayList.class);

	String JAVA_MAP_NAME = Type.getInternalName(Map.class);

	String JAVA_MAP_ENTRY_NAME = Type.getInternalName(Map.Entry.class);

	String JAVA_SET_NAME = Type.getInternalName(Set.class);

	String JAVA_ITERATOR_NAME = Type.getInternalName(Iterator.class);

	String JAVA_INTEGER_NAME = Type.getInternalName(Integer.class);

	String JAVA_INTEGER_VALUE_OF_METHOD_NAME = "valueOf";

	String JAVA_INTEGER_VALUE_OF_METHOD_DESC = GeneratorUtils.getMethodDescription(Integer.class, JAVA_INTEGER_VALUE_OF_METHOD_NAME, int.class);

	String JAVA_URI_NAME = Type.getInternalName(URI.class);

	String JAVA_URI_CREATE_METHOD_NAME = "create";

	String JAVA_URI_CREATE_METHOD_DESC = GeneratorUtils.getMethodDescription(URI.class, JAVA_URI_CREATE_METHOD_NAME, String.class);

	String JAVA_THROWABLE_NAME = Type.getInternalName(Throwable.class);

	String SYMBOL_STRUCT_NAME = Type.getInternalName(SymbolStruct.class);

	String SYMBOL_STRUCT_DESC = Type.getDescriptor(SymbolStruct.class);

	String SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME = "getLexicalValue";

	String SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME = "getDynamicValue";

	String SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_GET_VALUE_METHOD_NAME = "getValue";

	String SYMBOL_STRUCT_GET_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_VALUE_METHOD_NAME);

	String PACKAGE_STRUCT_NAME = Type.getInternalName(PackageStruct.class);

	String PACKAGE_STRUCT_DESC = Type.getDescriptor(PackageStruct.class);

	String PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME = "findPackage";

	String PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC = GeneratorUtils.getMethodDescription(PackageStruct.class, PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME, String.class);

	String PACKAGE_STRUCT_FIND_SYMBOL_METHOD_NAME = "findSymbol";

	String PACKAGE_STRUCT_FIND_SYMBOL_METHOD_DESC = GeneratorUtils.getMethodDescription(PackageStruct.class, PACKAGE_STRUCT_FIND_SYMBOL_METHOD_NAME, String.class);

	String PACKAGE_SYMBOL_STRUCT_NAME = Type.getInternalName(PackageSymbolStruct.class);

	String PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME = "getSymbol";

	String PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC = GeneratorUtils.getMethodDescription(PackageSymbolStruct.class, PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME);

	String FUNCTION_STRUCT_NAME = Type.getInternalName(FunctionStruct.class);

	String LISP_STRUCT_NAME = Type.getInternalName(LispStruct.class);

	String LISP_STRUCT_DESC = Type.getDescriptor(LispStruct.class);

	String VALUES_STRUCT_NAME = Type.getInternalName(ValuesStruct.class);

	String VALUES_STRUCT_DESC = Type.getDescriptor(ValuesStruct.class);

	String O_LAMBDA_LIST_BINDINGS_NAME = Type.getInternalName(OrdinaryLambdaListBindings.class);

	String O_LAMBDA_LIST_BINDINGS_DESC = Type.getDescriptor(OrdinaryLambdaListBindings.class);

	String ERROR_EXCEPTION_NAME = Type.getInternalName(ErrorException.class);

	String FUNCTION_PARAMETER_BINDING_NAME = Type.getInternalName(FunctionParameterBinding.class);

	String REQUIRED_BINDING_NAME = Type.getInternalName(RequiredBinding.class);

	String OPTIONAL_BINDING_NAME = Type.getInternalName(OptionalBinding.class);

	String REST_BINDING_NAME = Type.getInternalName(RestBinding.class);

	String KEY_BINDING_NAME = Type.getInternalName(KeyBinding.class);

	String AUX_BINDING_NAME = Type.getInternalName(AuxBinding.class);

	String SUPPLIED_P_BINDING_NAME = Type.getInternalName(SuppliedPBinding.class);

	String RETURN_FROM_EXCEPTION_NAME = Type.getInternalName(ReturnFromException.class);

	String RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME = "getName";

	String RETURN_FROM_EXCEPTION_GET_NAME_METHOD_DESC = GeneratorUtils.getMethodDescription(ReturnFromException.class, RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME);

	String RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME = "getResult";

	String RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_DESC = GeneratorUtils.getMethodDescription(ReturnFromException.class, RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME);

	String GO_EXCEPTION_NAME = Type.getInternalName(GoException.class);

	String GO_EXCEPTION_GET_TAG_INDEX_METHOD_NAME = "getTagIndex";

	String GO_EXCEPTION_GET_TAG_INDEX_METHOD_DESC = GeneratorUtils.getMethodDescription(GoException.class, GO_EXCEPTION_GET_TAG_INDEX_METHOD_NAME);

	String THROW_EXCEPTION_NAME = Type.getInternalName(ThrowException.class);

	String THROW_EXCEPTION_GET_CATCH_TAG_METHOD_NAME = "getCatchTag";

	String THROW_EXCEPTION_GET_CATCH_TAG_METHOD_DESC = GeneratorUtils.getMethodDescription(ThrowException.class, THROW_EXCEPTION_GET_CATCH_TAG_METHOD_NAME);

	String THROW_EXCEPTION_GET_RESULT_FORM_METHOD_NAME = "getResultForm";

	String THROW_EXCEPTION_GET_RESULT_FORM_METHOD_DESC = GeneratorUtils.getMethodDescription(ThrowException.class, THROW_EXCEPTION_GET_RESULT_FORM_METHOD_NAME);

}
