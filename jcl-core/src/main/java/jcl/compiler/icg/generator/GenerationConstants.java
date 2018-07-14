/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.DestructuringLambdaList;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.function.Closure;
import jcl.compiler.function.CompiledFunctionStruct;
import jcl.compiler.function.expanders.CompiledMacroFunctionExpander;
import jcl.lang.CharacterStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.internal.StructureObjectStructImpl;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.LispType;
import jcl.type.TypeBaseClass;
import jcl.type.TypeFactory;
import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apfloat.Apcomplex;
import org.apfloat.Aprational;
import org.objectweb.asm.Type;

public interface GenerationConstants {

	String JAVA_EXTENSION = ".java";

	String SINGLETON_INSTANCE = "INSTANCE";

	String INIT_METHOD_NAME = "<init>";

	String INIT_METHOD_DESC = "()V";

	String CLASS_INIT_METHOD_NAME = "<clinit>";

	String CLASS_INIT_METHOD_DESC = "()V";

	String JAVA_OBJECT_NAME = Type.getInternalName(Object.class);

	String JAVA_OBJECT_GET_CLASS_METHOD_NAME = "getClass";

	String JAVA_OBJECT_GET_CLASS_METHOD_DESC = CodeGenerators.getMethodDescription(Object.class, JAVA_OBJECT_GET_CLASS_METHOD_NAME);

	String JAVA_CLASS_NAME = Type.getInternalName(Class.class);

	String JAVA_METHOD_NAME = Type.getInternalName(Method.class);

	String JAVA_METHOD_INVOKE_METHOD_NAME = "invoke";

	String JAVA_METHOD_INVOKE_METHOD_DESC = CodeGenerators.getMethodDescription(Method.class, JAVA_METHOD_INVOKE_METHOD_NAME, Object.class, Object[].class);

	String JAVA_STREAM_NAME = Type.getInternalName(Stream.class);

	String JAVA_STREAM_COLLECT_METHOD_NAME = "collect";

	String JAVA_STREAM_COLLECT_METHOD_DESC = CodeGenerators.getMethodDescription(Stream.class, JAVA_STREAM_COLLECT_METHOD_NAME, Collector.class);

	String JAVA_COLLECTORS_NAME = Type.getInternalName(Collectors.class);

	String JAVA_COLLECTORS_TO_LIST_METHOD_NAME = "toList";

	String JAVA_COLLECTORS_TO_LIST_METHOD_DESC = CodeGenerators.getMethodDescription(Collectors.class, JAVA_COLLECTORS_TO_LIST_METHOD_NAME);

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

	String LISP_STRUCT_FACTORY_NAME = Type.getInternalName(LispStructFactory.class);

	String CHARACTER_STRUCT_NAME = Type.getInternalName(CharacterStruct.class);

	String CHARACTER_STRUCT_TO_LISP_CHARACTER_METHOD_NAME = "toLispCharacter";

	String CHARACTER_STRUCT_TO_LISP_CHARACTER_METHOD_DESC = CodeGenerators.getMethodDescription(CharacterStruct.class, CHARACTER_STRUCT_TO_LISP_CHARACTER_METHOD_NAME, int.class);

	/**
	 * Constant {@link String} containing the name for the {@link NILStruct} class.
	 */
	String NIL_STRUCT_NAME = Type.getInternalName(NILStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link NILStruct} class.
	 */
	String NIL_STRUCT_DESC = Type.getDescriptor(NILStruct.class);

	String CONS_STRUCT_NAME = Type.getInternalName(ConsStruct.class);

	String CONS_STRUCT_FACTORY_TO_CONS_METHOD_NAME = "toLispCons";

	String CONS_STRUCT_FACTORY_TO_CONS_METHOD_DESC = CodeGenerators.getMethodDescription(ConsStruct.class, CONS_STRUCT_FACTORY_TO_CONS_METHOD_NAME, LispStruct.class, LispStruct.class);

	String LISP_STRUCT_FACTORY_TO_PATHNAME_URI_METHOD_NAME = "toPathname";

	String LISP_STRUCT_FACTORY_TO_PATHNAME_URI_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_PATHNAME_URI_METHOD_NAME, URI.class);

	String LISP_STRUCT_FACTORY_TO_BIT_VECTOR_LIST_METHOD_NAME = "toBitVector";

	String LISP_STRUCT_FACTORY_TO_BIT_VECTOR_LIST_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_BIT_VECTOR_LIST_METHOD_NAME, List.class);

	String LISP_STRUCT_FACTORY_TO_ARRAY_METHOD_NAME = "toArray";

	String LISP_STRUCT_FACTORY_TO_ARRAY_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_ARRAY_METHOD_NAME, List.class, List.class);

	String LISP_STRUCT_FACTORY_TO_VECTOR_METHOD_NAME = "toVector";

	String LISP_STRUCT_FACTORY_TO_VECTOR_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_VECTOR_METHOD_NAME, List.class);

	String STRING_STRUCT_NAME = Type.getInternalName(StringStruct.class);

	String STRING_STRUCT_TO_LISP_STRING_METHOD_NAME = "toLispString";

	String STRING_STRUCT_TO_LISP_STRING_METHOD_DESC = CodeGenerators.getMethodDescription(StringStruct.class, STRING_STRUCT_TO_LISP_STRING_METHOD_NAME, String.class);

	String LISP_STRUCT_FACTORY_TO_COMPLEX_METHOD_NAME = "toComplex";

	String LISP_STRUCT_FACTORY_TO_COMPLEX_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_COMPLEX_METHOD_NAME, Apcomplex.class);

	String LISP_STRUCT_FACTORY_TO_RATIO_METHOD_NAME = "toRatio";

	String LISP_STRUCT_FACTORY_TO_RATIO_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_RATIO_METHOD_NAME, Aprational.class);

	String LISP_STRUCT_FACTORY_TO_SYMBOL_METHOD_NAME = "toSymbol";

	String LISP_STRUCT_FACTORY_TO_SYMBOL_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_SYMBOL_METHOD_NAME, String.class);

	String LISP_STRUCT_FACTORY_TO_JAVA_NAME_METHOD_NAME = "toJavaName";

	String LISP_STRUCT_FACTORY_TO_JAVA_NAME_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_JAVA_NAME_METHOD_NAME, String.class);

	String LISP_STRUCT_FACTORY_TO_JAVA_REFLECTION_METHOD_METHOD_NAME = "toJavaReflectionMethod";

	String LISP_STRUCT_FACTORY_TO_JAVA_REFLECTION_METHOD_METHOD_DESC = CodeGenerators.getMethodDescription(LispStructFactory.class, LISP_STRUCT_FACTORY_TO_JAVA_REFLECTION_METHOD_METHOD_NAME, String.class, Class.class, Class[].class);

	String SYMBOL_STRUCT_NAME = Type.getInternalName(SymbolStruct.class);

	String SYMBOL_STRUCT_DESC = Type.getDescriptor(SymbolStruct.class);

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

	String SYMBOL_STRUCT_SET_MACRO_FUNCTION_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_SET_MACRO_FUNCTION_EXPANDER_METHOD_NAME, MacroFunctionExpanderInter.class);

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

	String COMPILED_FUNCTION_STRUCT_NAME = Type.getInternalName(CompiledFunctionStruct.class);

	String COMPILED_FUNCTION_STRUCT_INIT_CLOSURE_DESC = CodeGenerators.getConstructorDescription(CompiledFunctionStruct.class, Closure.class);

	String COMPILED_FUNCTION_STRUCT_INIT_STRING_CLOSURE_DESC = CodeGenerators.getConstructorDescription(CompiledFunctionStruct.class, String.class, Closure.class);

	String FUNCTION_STRUCT_APPLY_METHOD_NAME = "apply";

	String FUNCTION_STRUCT_APPLY_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_APPLY_METHOD_NAME, LispStruct[].class);

	String COMPILED_MACRO_FUNCTION_EXPANDER_NAME = Type.getInternalName(CompiledMacroFunctionExpander.class);

	String COMPILED_MACRO_FUNCTION_EXPANDER_CLASS_SIGNATURE = "Ljcl/compiler/function/expanders/CompiledMacroFunctionExpander<Ljcl/lang/LispStruct;>;";

	String LISP_STRUCT_NAME = Type.getInternalName(LispStruct.class);

	String LISP_STRUCT_DESC = Type.getDescriptor(LispStruct.class);

	String LISP_STRUCT_EQ_METHOD_NAME = "eq";

	String LISP_STRUCT_EQ_METHOD_DESC = CodeGenerators.getMethodDescription(LispStruct.class, LISP_STRUCT_EQ_METHOD_NAME, LispStruct.class);

	String LISP_STRUCT_ARRAY_DESC = Type.getDescriptor(LispStruct[].class);

	String VALUES_STRUCT_NAME = Type.getInternalName(ValuesStruct.class);

	String VALUES_STRUCT_ADD_VALUES_TO_LIST_METHOD_NAME = "addValuesToList";

	String VALUES_STRUCT_ADD_VALUES_TO_LIST_METHOD_DESC = CodeGenerators.getMethodDescription(ValuesStruct.class, VALUES_STRUCT_ADD_VALUES_TO_LIST_METHOD_NAME, List.class, LispStruct.class);

	String VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_NAME = "extractPrimaryValue";

	String VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(ValuesStruct.class, VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_NAME, LispStruct.class);

	String CLOSURE_NAME = Type.getInternalName(Closure.class);

	String CLOSURE_INIT_CLOSURE_DESC = CodeGenerators.getConstructorDescription(Closure.class, Closure.class);

	String CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME = "getFunctionBindings";

	String CLOSURE_GET_FUNCTION_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(Closure.class, CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME);

	String CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME = "getSymbolBindings";

	String CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC = CodeGenerators.getMethodDescription(Closure.class, CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME);

	String PROGRAM_ERROR_EXCEPTION_NAME = Type.getInternalName(ProgramErrorException.class);

	String PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC = CodeGenerators.getConstructorDescription(ProgramErrorException.class, String.class);

	String REQUIRED_BINDING_NAME = Type.getInternalName(RequiredParameter.class);

	String REQUIRED_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(RequiredParameter.class, SymbolStruct.class, boolean.class);

	String REQUIRED_BINDING_DESTRUCTURING_INIT_DESC = CodeGenerators.getConstructorDescription(RequiredParameter.class, SymbolStruct.class, DestructuringLambdaList.class, boolean.class);

	String OPTIONAL_BINDING_NAME = Type.getInternalName(OptionalParameter.class);

	String OPTIONAL_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(OptionalParameter.class, SymbolStruct.class, LispStruct.class, boolean.class, SuppliedPParameter.class);

	String OPTIONAL_BINDING_DESTRUCTURING_INIT_DESC = CodeGenerators.getConstructorDescription(OptionalParameter.class, SymbolStruct.class, DestructuringLambdaList.class, LispStruct.class, boolean.class, SuppliedPParameter.class);

	String REST_BINDING_NAME = Type.getInternalName(RestParameter.class);

	String REST_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(RestParameter.class, SymbolStruct.class, boolean.class);

	String REST_BINDING_DESTRUCTURING_INIT_DESC = CodeGenerators.getConstructorDescription(RestParameter.class, SymbolStruct.class, DestructuringLambdaList.class, boolean.class);

	String KEY_BINDING_NAME = Type.getInternalName(KeyParameter.class);

	String KEY_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(KeyParameter.class, SymbolStruct.class, LispStruct.class, boolean.class, SymbolStruct.class, SuppliedPParameter.class);

	String KEY_BINDING_DESTRUCTURING_INIT_DESC = CodeGenerators.getConstructorDescription(KeyParameter.class, SymbolStruct.class, DestructuringLambdaList.class, LispStruct.class, boolean.class, SymbolStruct.class, SuppliedPParameter.class);

	String AUX_BINDING_NAME = Type.getInternalName(AuxParameter.class);

	String AUX_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(AuxParameter.class, SymbolStruct.class, LispStruct.class, boolean.class);

	String AUX_BINDING_DESTRUCTURING_INIT_DESC = CodeGenerators.getConstructorDescription(AuxParameter.class, SymbolStruct.class, DestructuringLambdaList.class, LispStruct.class, boolean.class);

	String SUPPLIED_P_BINDING_NAME = Type.getInternalName(SuppliedPParameter.class);

	String SUPPLIED_P_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(SuppliedPParameter.class, SymbolStruct.class, boolean.class);

	String WHOLE_BINDING_NAME = Type.getInternalName(WholeParameter.class);

	String WHOLE_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(WholeParameter.class, SymbolStruct.class, boolean.class);

	String ENVIRONMENT_BINDING_NAME = Type.getInternalName(EnvironmentParameter.class);

	String ENVIRONMENT_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(EnvironmentParameter.class, SymbolStruct.class);

	String BODY_BINDING_NAME = Type.getInternalName(BodyParameter.class);

	String BODY_BINDING_INIT_DESC = CodeGenerators.getConstructorDescription(BodyParameter.class, SymbolStruct.class, boolean.class);

	String BODY_BINDING_DESTRUCTURING_INIT_DESC = CodeGenerators.getConstructorDescription(BodyParameter.class, SymbolStruct.class, DestructuringLambdaList.class, boolean.class);

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

	String LIST_STRUCT_NAME = Type.getInternalName(ListStruct.class);

	String LIST_STRUCT_STREAM_METHOD_NAME = "stream";

	String LIST_STRUCT_STREAM_METHOD_DESC = CodeGenerators.getMethodDescription(ListStruct.class, LIST_STRUCT_STREAM_METHOD_NAME);

	String STRUCTURE_OBJECT_STRUCT_NAME = Type.getInternalName(StructureObjectStructImpl.class);

	String TYPE_BASE_CLASS_NAME = Type.getInternalName(TypeBaseClass.class);

	String TYPE_BASE_CLASS_INIT_STRING_DESC = CodeGenerators.getConstructorDescription(TypeBaseClass.class, String.class);

	String ATOMIC_TYPE_SPECIFIER_NAME = Type.getInternalName(AtomicTypeSpecifier.class);

	String TYPE_FACTORY_NAME = Type.getInternalName(TypeFactory.class);

	String LISP_TYPE_TYPE_EQUALS_METHOD_NAME = "typeEquals";

	String LISP_TYPE_TYPE_EQUALS_METHOD_DESC = CodeGenerators.getMethodDescription(LispType.class, LISP_TYPE_TYPE_EQUALS_METHOD_NAME, Object.class);
}
