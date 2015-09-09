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
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.Closure;
import jcl.functions.FunctionParameterBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.structures.StructureClassStruct;
import jcl.structures.StructureObjectStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TypeBaseClass;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.objectweb.asm.Type;

interface GenerationConstants {

	String JAVA_EXTENSION = ".java";

	String SERIAL_VERSION_UID_FIELD = "serialVersionUID";

	String SINGLETON_INSTANCE = "INSTANCE";

	String JAVA_LONG_TYPE_NAME = Type.LONG_TYPE.getDescriptor();

	String INIT_METHOD_NAME = "<init>";

	String INIT_METHOD_DESC = "()V";

	String CLASS_INIT_METHOD_NAME = "<clinit>";

	String CLASS_INIT_METHOD_DESC = "()V";

	String JAVA_OBJECT_NAME = Type.getInternalName(Object.class);

	String JAVA_EQUALS_METHOD_NAME = "equals";

	String JAVA_EQUALS_METHOD_DESC = GeneratorUtils.getMethodDescription(Object.class, JAVA_EQUALS_METHOD_NAME, Object.class);

	String JAVA_HASH_CODE_METHOD_NAME = "hashCode";

	String JAVA_HASH_CODE_METHOD_DESC = GeneratorUtils.getMethodDescription(Object.class, JAVA_HASH_CODE_METHOD_NAME);

	String JAVA_LIST_NAME = Type.getInternalName(List.class);

	String JAVA_LIST_ADD_METHOD_NAME = "add";

	String JAVA_LIST_ADD_METHOD_DESC = GeneratorUtils.getMethodDescription(List.class, JAVA_LIST_ADD_METHOD_NAME, Object.class);

	String JAVA_LIST_GET_METHOD_NAME = "get";

	String JAVA_LIST_GET_METHOD_DESC = GeneratorUtils.getMethodDescription(List.class, JAVA_LIST_GET_METHOD_NAME, int.class);

	String JAVA_LIST_SIZE_METHOD_NAME = "size";

	String JAVA_LIST_SIZE_METHOD_DESC = GeneratorUtils.getMethodDescription(List.class, JAVA_LIST_SIZE_METHOD_NAME);

	String JAVA_LIST_ITERATOR_METHOD_NAME = "iterator";

	String JAVA_LIST_ITERATOR_METHOD_DESC = GeneratorUtils.getMethodDescription(List.class, JAVA_LIST_ITERATOR_METHOD_NAME);

	String JAVA_LIST_TO_ARRAY_METHOD_NAME = "toArray";

	String JAVA_LIST_TO_ARRAY_METHOD_DESC = GeneratorUtils.getMethodDescription(List.class, JAVA_LIST_TO_ARRAY_METHOD_NAME, Object[].class);

	String JAVA_ARRAY_LIST_NAME = Type.getInternalName(ArrayList.class);

	String JAVA_ARRAY_LIST_INIT_DESC = GeneratorUtils.getConstructorDescription(ArrayList.class);

	String JAVA_MAP_NAME = Type.getInternalName(Map.class);

	String JAVA_MAP_DESC = Type.getDescriptor(Map.class);

	String JAVA_MAP_PUT_METHOD_NAME = "put";

	String JAVA_MAP_PUT_METHOD_DESC = GeneratorUtils.getMethodDescription(Map.class, JAVA_MAP_PUT_METHOD_NAME, Object.class, Object.class);

	String JAVA_MAP_ENTRY_SET_METHOD_NAME = "entrySet";

	String JAVA_MAP_ENTRY_SET_METHOD_DESC = GeneratorUtils.getMethodDescription(Map.class, JAVA_MAP_ENTRY_SET_METHOD_NAME);

	String JAVA_MAP_KEY_SET_METHOD_NAME = "keySet";

	String JAVA_MAP_KEY_SET_METHOD_DESC = GeneratorUtils.getMethodDescription(Map.class, JAVA_MAP_KEY_SET_METHOD_NAME);

	String JAVA_MAP_ENTRY_NAME = Type.getInternalName(Map.Entry.class);

	String JAVA_MAP_ENTRY_GET_KEY_METHOD_NAME = "getKey";

	String JAVA_MAP_ENTRY_GET_KEY_METHOD_DESC = GeneratorUtils.getMethodDescription(Map.Entry.class, JAVA_MAP_ENTRY_GET_KEY_METHOD_NAME);

	String JAVA_MAP_ENTRY_GET_VALUE_METHOD_NAME = "getValue";

	String JAVA_MAP_ENTRY_GET_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(Map.Entry.class, JAVA_MAP_ENTRY_GET_VALUE_METHOD_NAME);

	String JAVA_SET_NAME = Type.getInternalName(Set.class);

	String JAVA_SET_ITERATOR_METHOD_NAME = "iterator";

	String JAVA_SET_ITERATOR_METHOD_DESC = GeneratorUtils.getMethodDescription(Set.class, JAVA_SET_ITERATOR_METHOD_NAME);

	String JAVA_ITERATOR_NAME = Type.getInternalName(Iterator.class);

	String JAVA_ITERATOR_HAS_NEXT_METHOD_NAME = "hasNext";

	String JAVA_ITERATOR_HAS_NEXT_METHOD_DESC = GeneratorUtils.getMethodDescription(Iterator.class, JAVA_ITERATOR_HAS_NEXT_METHOD_NAME);

	String JAVA_ITERATOR_NEXT_METHOD_NAME = "next";

	String JAVA_ITERATOR_NEXT_METHOD_DESC = GeneratorUtils.getMethodDescription(Iterator.class, JAVA_ITERATOR_NEXT_METHOD_NAME);

	String JAVA_INTEGER_NAME = Type.getInternalName(Integer.class);

	String JAVA_INTEGER_VALUE_OF_METHOD_NAME = "valueOf";

	String JAVA_INTEGER_VALUE_OF_METHOD_DESC = GeneratorUtils.getMethodDescription(Integer.class, JAVA_INTEGER_VALUE_OF_METHOD_NAME, int.class);

	String JAVA_URI_NAME = Type.getInternalName(URI.class);

	String JAVA_URI_CREATE_METHOD_NAME = "create";

	String JAVA_URI_CREATE_METHOD_DESC = GeneratorUtils.getMethodDescription(URI.class, JAVA_URI_CREATE_METHOD_NAME, String.class);

	String JAVA_THROWABLE_NAME = Type.getInternalName(Throwable.class);

	String JAVA_STRING_BUILDER_NAME = Type.getInternalName(StringBuilder.class);

	String JAVA_STRING_BUILDER_INIT_DESC = GeneratorUtils.getConstructorDescription(StringBuilder.class);

	String JAVA_STRING_BUILDER_APPEND_METHOD_NAME = "append";

	String JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC = GeneratorUtils.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_APPEND_METHOD_NAME, String.class);

	String JAVA_STRING_BUILDER_APPEND_OBJECT_METHOD_DESC = GeneratorUtils.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_APPEND_METHOD_NAME, Object.class);

	String JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME = "toString";

	String JAVA_STRING_BUILDER_TO_STRING_METHOD_DESC = GeneratorUtils.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME);

	String HASH_CODE_BUILDER_NAME = "org/apache/commons/lang3/builder/HashCodeBuilder";

	String HASH_CODE_BUILDER_APPEND_SUPER_METHOD_NAME = "appendSuper";

	String HASH_CODE_BUILDER_APPEND_SUPER_METHOD_DESC = GeneratorUtils.getMethodDescription(StringBuilder.class, HASH_CODE_BUILDER_APPEND_SUPER_METHOD_NAME, int.class);

	String HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_NAME = "toHashCode";

	String HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_DESC = GeneratorUtils.getMethodDescription(StringBuilder.class, HASH_CODE_BUILDER_TO_HASH_CODE_METHOD_NAME);

	String SYMBOL_STRUCT_NAME = Type.getInternalName(SymbolStruct.class);

	String SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME = "getLexicalValue";

	String SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_NAME = "setLexicalValue";

	String SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME = "bindLexicalValue";

	String SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME = "unbindLexicalValue";

	String SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME = "getDynamicValue";

	String SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_NAME = "setDynamicValue";

	String SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME = "bindDynamicValue";

	String SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME = "unbindDynamicValue";

	String SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_GET_VALUE_METHOD_NAME = "getValue";

	String SYMBOL_STRUCT_GET_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_VALUE_METHOD_NAME);

	String SYMBOL_STRUCT_SET_VALUE_METHOD_NAME = "setValue";

	String SYMBOL_STRUCT_SET_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_VALUE_METHOD_NAME, LispStruct.class);

	String SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME = "getFunction";

	String SYMBOL_STRUCT_GET_FUNCTION_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME);

	String SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME = "bindFunction";

	String SYMBOL_STRUCT_BIND_FUNCTION_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME, FunctionStruct.class);

	String SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME = "unbindFunction";

	String SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME);

	String SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME = "setStructureClass";

	String SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_DESC = GeneratorUtils.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_STRUCTURE_CLASS_METHOD_NAME, StructureClassStruct.class);

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

	String FUNCTION_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(FunctionStruct.class);

	String FUNCTION_STRUCT_INIT_STRING_CLOSURE_DESC = GeneratorUtils.getConstructorDescription(FunctionStruct.class, String.class, Closure.class);

	String FUNCTION_STRUCT_GET_FUNCTION_BINDINGS_METHOD_NAME = "getFunctionBindings";

	// NOTE: Can't use the GeneratorUtils here because this method has 'PROTECTED' access.
	String FUNCTION_STRUCT_GET_FUNCTION_BINDINGS_METHOD_DESC = "([Ljcl/LispStruct;)Ljava/util/List;";

	String FUNCTION_STRUCT_GET_CLOSURE_FUNCTION_BINDINGS_METHOD_NAME = "getClosureFunctionBindings";

	String FUNCTION_STRUCT_GET_CLOSURE_FUNCTION_BINDINGS_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_GET_CLOSURE_FUNCTION_BINDINGS_METHOD_NAME);

	String FUNCTION_STRUCT_GET_CLOSURE_SYMBOL_BINDINGS_METHOD_NAME = "getClosureSymbolBindings";

	String FUNCTION_STRUCT_GET_CLOSURE_SYMBOL_BINDINGS_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_GET_CLOSURE_SYMBOL_BINDINGS_METHOD_NAME);

	String FUNCTION_STRUCT_GET_CLOSURE_METHOD_NAME = "getClosure";

	String FUNCTION_STRUCT_GET_CLOSURE_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_GET_CLOSURE_METHOD_NAME);

	String FUNCTION_STRUCT_APPLY_METHOD_NAME = "apply";

	String FUNCTION_STRUCT_APPLY_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_APPLY_METHOD_NAME, LispStruct[].class);

	String LISP_STRUCT_NAME = Type.getInternalName(LispStruct.class);

	String LISP_STRUCT_DESC = Type.getDescriptor(LispStruct.class);

	String LISP_STRUCT_ARRAY_DESC = Type.getDescriptor(LispStruct[].class);

	String VALUES_STRUCT_NAME = Type.getInternalName(ValuesStruct.class);

	String VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME = "getPrimaryValue";

	String VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(ValuesStruct.class, VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME);

	String VALUES_STRUCT_GET_VALUES_LIST_METHOD_NAME = "getValuesList";

	String VALUES_STRUCT_GET_VALUES_LIST_METHOD_DESC = GeneratorUtils.getMethodDescription(ValuesStruct.class, VALUES_STRUCT_GET_VALUES_LIST_METHOD_NAME);

	String CLOSURE_NAME = Type.getInternalName(Closure.class);

	String CLOSURE_DESC = Type.getDescriptor(Closure.class);

	String CLOSURE_INIT_CLOSURE_DESC = GeneratorUtils.getConstructorDescription(Closure.class, Closure.class);

	String CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME = "getFunctionBindings";

	String CLOSURE_GET_FUNCTION_BINDINGS_METHOD_DESC = GeneratorUtils.getMethodDescription(Closure.class, CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME);

	String CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME = "getSymbolBindings";

	String CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC = GeneratorUtils.getMethodDescription(Closure.class, CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME);

	String O_LAMBDA_LIST_BINDINGS_NAME = Type.getInternalName(OrdinaryLambdaListBindings.class);

	String O_LAMBDA_LIST_BINDINGS_DESC = Type.getDescriptor(OrdinaryLambdaListBindings.class);

	String O_LAMBDA_LIST_BINDINGS_INIT_DESC = GeneratorUtils.getConstructorDescription(OrdinaryLambdaListBindings.class, List.class, List.class, RestBinding.class, List.class, List.class, boolean.class);

	String ERROR_EXCEPTION_NAME = Type.getInternalName(ErrorException.class);

	String PROGRAM_ERROR_EXCEPTION_NAME = Type.getInternalName(ProgramErrorException.class);

	String PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC = GeneratorUtils.getConstructorDescription(ProgramErrorException.class, String.class);

	String FUNCTION_PARAMETER_BINDING_NAME = Type.getInternalName(FunctionParameterBinding.class);

	String FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_NAME = "getParameterSymbol";

	String FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionParameterBinding.class, FUNCTION_PARAMETER_BINDING_GET_PARAMETER_SYMBOL_METHOD_NAME);

	String FUNCTION_PARAMETER_BINDING_GET_PARAMETER_VALUE_METHOD_NAME = "getParameterValue";

	String FUNCTION_PARAMETER_BINDING_GET_PARAMETER_VALUE_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionParameterBinding.class, FUNCTION_PARAMETER_BINDING_GET_PARAMETER_VALUE_METHOD_NAME);

	String FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_NAME = "isSpecial";

	String FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_DESC = GeneratorUtils.getMethodDescription(FunctionParameterBinding.class, FUNCTION_PARAMETER_BINDING_IS_SPECIAL_METHOD_NAME);

	String REQUIRED_BINDING_NAME = Type.getInternalName(RequiredBinding.class);

	String REQUIRED_BINDING_INIT_DESC = GeneratorUtils.getConstructorDescription(RequiredBinding.class, SymbolStruct.class, boolean.class);

	String OPTIONAL_BINDING_NAME = Type.getInternalName(OptionalBinding.class);

	String OPTIONAL_BINDING_INIT_DESC = GeneratorUtils.getConstructorDescription(OptionalBinding.class, SymbolStruct.class, LispStruct.class, boolean.class, SuppliedPBinding.class);

	String REST_BINDING_NAME = Type.getInternalName(RestBinding.class);

	String REST_BINDING_INIT_DESC = GeneratorUtils.getConstructorDescription(RestBinding.class, SymbolStruct.class, boolean.class);

	String KEY_BINDING_NAME = Type.getInternalName(KeyBinding.class);

	String KEY_BINDING_INIT_DESC = GeneratorUtils.getConstructorDescription(KeyBinding.class, SymbolStruct.class, LispStruct.class, boolean.class, SymbolStruct.class, SuppliedPBinding.class);

	String AUX_BINDING_NAME = Type.getInternalName(AuxBinding.class);

	String AUX_BINDING_INIT_DESC = GeneratorUtils.getConstructorDescription(AuxBinding.class, SymbolStruct.class, LispStruct.class, boolean.class);

	String SUPPLIED_P_BINDING_NAME = Type.getInternalName(SuppliedPBinding.class);

	String SUPPLIED_P_BINDING_INIT_DESC = GeneratorUtils.getConstructorDescription(SuppliedPBinding.class, SymbolStruct.class, boolean.class);

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

	String CONS_STRUCT_NAME = Type.getInternalName(ConsStruct.class);

	String CONS_STRUCT_INIT_CAR_DESC = GeneratorUtils.getConstructorDescription(ConsStruct.class, LispStruct.class);

	String CONS_STRUCT_INIT_CAR_CDR_DESC = GeneratorUtils.getConstructorDescription(ConsStruct.class, LispStruct.class, LispStruct.class);

	String LIST_STRUCT_NAME = Type.getInternalName(ListStruct.class);

	String LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME = "getAsJavaList";

	String LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_DESC = GeneratorUtils.getMethodDescription(ListStruct.class, LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME);

	String STRUCTURE_OBJECT_STRUCT_NAME = Type.getInternalName(StructureObjectStruct.class);

	String TYPE_BASE_CLASS_NAME = Type.getInternalName(TypeBaseClass.class);

	String TYPE_BASE_CLASS_INIT_STRING_DESC = GeneratorUtils.getConstructorDescription(TypeBaseClass.class, String.class);

	String ATOMIC_TYPE_SPECIFIER_NAME = Type.getInternalName(AtomicTypeSpecifier.class);

	String TYPE_FACTORY_NAME = Type.getInternalName(TypeFactory.class);
}
