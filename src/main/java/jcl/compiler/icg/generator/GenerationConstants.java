/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.compiler.environment.Environment;
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
import jcl.compiler.function.CompiledFunctionStruct;
import jcl.compiler.function.expanders.CompiledMacroFunctionExpander;
import jcl.lang.ArrayStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public interface GenerationConstants {

	int JAVA_VERSION = Opcodes.V19;

	String JAVA_EXTENSION = ".java";

	String SINGLETON_INSTANCE = "INSTANCE";

	String INIT_METHOD_NAME = "<init>";

	String INIT_METHOD_DESC = "()V";

	String CLASS_INIT_METHOD_NAME = "<clinit>";

	String CLASS_INIT_METHOD_DESC = "()V";

	String JAVA_OBJECT_NAME = Type.getInternalName(Object.class);

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

	String JAVA_STRING_BUILDER_NAME = Type.getInternalName(StringBuilder.class);

	String JAVA_STRING_BUILDER_INIT_DESC = CodeGenerators.getConstructorDescription(StringBuilder.class);

	String JAVA_STRING_BUILDER_APPEND_METHOD_NAME = "append";

	String JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC = CodeGenerators.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_APPEND_METHOD_NAME, String.class);

	String JAVA_STRING_BUILDER_APPEND_OBJECT_METHOD_DESC = CodeGenerators.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_APPEND_METHOD_NAME, Object.class);

	String JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME = "toString";

	String JAVA_STRING_BUILDER_TO_STRING_METHOD_DESC = CodeGenerators.getMethodDescription(StringBuilder.class, JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME);

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

	/**
	 * Constant {@link String} containing the name for the {@link TStruct} class.
	 */
	String T_STRUCT_NAME = Type.getInternalName(TStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link TStruct} class.
	 */
	String T_STRUCT_DESC = Type.getDescriptor(TStruct.class);

	String CONS_STRUCT_NAME = Type.getInternalName(ConsStruct.class);

	String CONS_STRUCT_TO_CONS_METHOD_NAME = "toLispCons";

	String CONS_STRUCT_TO_CONS_METHOD_DESC = CodeGenerators.getMethodDescription(ConsStruct.class, CONS_STRUCT_TO_CONS_METHOD_NAME, LispStruct.class, LispStruct.class);

	String BIT_VECTOR_STRUCT_NAME = Type.getInternalName(BitVectorStruct.class);

	String BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME = "toLispBitVector";

	String BIT_VECTOR_STRUCT_TO_SIMPLE_BIT_VECTOR_METHOD_DESC = CodeGenerators.getMethodDescription(BitVectorStruct.class, BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME, IntegerStruct.class, List.class);

	String BIT_VECTOR_STRUCT_TO_COMPLEX_BIT_VECTOR_CONTENTS_METHOD_DESC = CodeGenerators.getMethodDescription(BitVectorStruct.class, BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME, IntegerStruct.class, List.class, BooleanStruct.class, IntegerStruct.class);

	String BIT_VECTOR_STRUCT_TO_COMPLEX_BIT_VECTOR_DISPLACED_METHOD_DESC = CodeGenerators.getMethodDescription(BitVectorStruct.class, BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME, IntegerStruct.class, ArrayStruct.class, IntegerStruct.class, BooleanStruct.class, IntegerStruct.class);

	String ARRAY_STRUCT_NAME = Type.getInternalName(ArrayStruct.class);

	String ARRAY_STRUCT_TO_ARRAY_METHOD_NAME = "toLispArray";

	String ARRAY_STRUCT_TO_SIMPLE_ARRAY_METHOD_DESC = CodeGenerators.getMethodDescription(ArrayStruct.class, ARRAY_STRUCT_TO_ARRAY_METHOD_NAME, List.class, SymbolStruct.class, List.class);

	String ARRAY_STRUCT_TO_COMPLEX_ARRAY_CONTENTS_METHOD_DESC = CodeGenerators.getMethodDescription(ArrayStruct.class, ARRAY_STRUCT_TO_ARRAY_METHOD_NAME, List.class, SymbolStruct.class, List.class, BooleanStruct.class);

	String ARRAY_STRUCT_TO_COMPLEX_ARRAY_DISPLACED_METHOD_DESC = CodeGenerators.getMethodDescription(ArrayStruct.class, ARRAY_STRUCT_TO_ARRAY_METHOD_NAME, List.class, SymbolStruct.class, ArrayStruct.class, IntegerStruct.class, BooleanStruct.class);

	String VECTOR_STRUCT_NAME = Type.getInternalName(VectorStruct.class);

	String VECTOR_STRUCT_TO_VECTOR_METHOD_NAME = "toLispVector";

	String VECTOR_STRUCT_TO_NIL_VECTOR_METHOD_DESC = CodeGenerators.getMethodDescription(VectorStruct.class, VECTOR_STRUCT_TO_VECTOR_METHOD_NAME, IntegerStruct.class);

	String VECTOR_STRUCT_TO_SIMPLE_VECTOR_METHOD_DESC = CodeGenerators.getMethodDescription(VectorStruct.class, VECTOR_STRUCT_TO_VECTOR_METHOD_NAME, IntegerStruct.class, SymbolStruct.class, List.class);

	String VECTOR_STRUCT_TO_COMPLEX_VECTOR_CONTENTS_METHOD_DESC = CodeGenerators.getMethodDescription(VectorStruct.class, VECTOR_STRUCT_TO_VECTOR_METHOD_NAME, IntegerStruct.class, SymbolStruct.class, List.class, BooleanStruct.class, IntegerStruct.class);

	String VECTOR_STRUCT_TO_COMPLEX_VECTOR_DISPLACED_METHOD_DESC = CodeGenerators.getMethodDescription(VectorStruct.class, VECTOR_STRUCT_TO_VECTOR_METHOD_NAME, IntegerStruct.class, SymbolStruct.class, ArrayStruct.class, IntegerStruct.class, BooleanStruct.class, IntegerStruct.class);

	String STRING_STRUCT_NAME = Type.getInternalName(StringStruct.class);

	String STRING_STRUCT_TO_LISP_STRING_METHOD_NAME = "toLispString";

	String STRING_STRUCT_TO_SIMPLE_STRING_METHOD_DESC = CodeGenerators.getMethodDescription(StringStruct.class, STRING_STRUCT_TO_LISP_STRING_METHOD_NAME, IntegerStruct.class, SymbolStruct.class, String.class);

	String STRING_STRUCT_TO_COMPLEX_STRING_CONTENTS_METHOD_DESC = CodeGenerators.getMethodDescription(StringStruct.class, STRING_STRUCT_TO_LISP_STRING_METHOD_NAME, IntegerStruct.class, SymbolStruct.class, String.class, BooleanStruct.class, IntegerStruct.class);

	String STRING_STRUCT_TO_COMPLEX_STRING_DISPLACED_METHOD_DESC = CodeGenerators.getMethodDescription(StringStruct.class, STRING_STRUCT_TO_LISP_STRING_METHOD_NAME, IntegerStruct.class, SymbolStruct.class, ArrayStruct.class, IntegerStruct.class, BooleanStruct.class, IntegerStruct.class);

	String COMMON_LISP_SYMBOLS_NAME = Type.getInternalName(CommonLispSymbols.class);

	String SYMBOL_STRUCT_NAME = Type.getInternalName(SymbolStruct.class);

	String SYMBOL_STRUCT_DESC = Type.getDescriptor(SymbolStruct.class);

	String SYMBOL_STRUCT_TO_SYMBOL_METHOD_NAME = "toLispSymbol";

	String SYMBOL_STRUCT_TO_SYMBOL_METHOD_DESC = CodeGenerators.getMethodDescription(SymbolStruct.class, SYMBOL_STRUCT_TO_SYMBOL_METHOD_NAME, String.class);

	String ENVIRONMENT_NAME = Type.getInternalName(Environment.class);

	String ENVIRONMENT_NULL_NAME = "NULL";

	String ENVIRONMENT_DESC = Type.getDescriptor(Environment.class);

	String ENVIRONMENT_INIT_ENVIRONMENT_DESC = CodeGenerators.getConstructorDescription(Environment.class, Environment.class);

	String ENVIRONMENT_GET_LEXICAL_SYMBOL_VALUE_METHOD_NAME = "getLexicalSymbolValue";

	String ENVIRONMENT_GET_LEXICAL_SYMBOL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_GET_LEXICAL_SYMBOL_VALUE_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_SET_LEXICAL_SYMBOL_VALUE_METHOD_NAME = "setLexicalSymbolValue";

	String ENVIRONMENT_SET_LEXICAL_SYMBOL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_SET_LEXICAL_SYMBOL_VALUE_METHOD_NAME, SymbolStruct.class, LispStruct.class);

	String ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_NAME = "bindLexicalValue";

	String ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_NAME, SymbolStruct.class, LispStruct.class);

	String ENVIRONMENT_UNBIND_LEXICAL_VALUE_METHOD_NAME = "unbindLexicalValue";

	String ENVIRONMENT_UNBIND_LEXICAL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_UNBIND_LEXICAL_VALUE_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_GET_DYNAMIC_SYMBOL_VALUE_METHOD_NAME = "getDynamicSymbolValue";

	String ENVIRONMENT_GET_DYNAMIC_SYMBOL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_GET_DYNAMIC_SYMBOL_VALUE_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_SET_DYNAMIC_SYMBOL_VALUE_METHOD_NAME = "setDynamicSymbolValue";

	String ENVIRONMENT_SET_DYNAMIC_SYMBOL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_SET_DYNAMIC_SYMBOL_VALUE_METHOD_NAME, SymbolStruct.class, LispStruct.class);

	String ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_NAME = "bindDynamicValue";

	String ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_NAME, SymbolStruct.class, LispStruct.class);

	String ENVIRONMENT_UNBIND_DYNAMIC_VALUE_METHOD_NAME = "unbindDynamicValue";

	String ENVIRONMENT_UNBIND_DYNAMIC_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_UNBIND_DYNAMIC_VALUE_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_GET_SYMBOL_VALUE_METHOD_NAME = "getSymbolValue";

	String ENVIRONMENT_GET_SYMBOL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_GET_SYMBOL_VALUE_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_SET_SYMBOL_VALUE_METHOD_NAME = "setSymbolValue";

	String ENVIRONMENT_SET_SYMBOL_VALUE_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_SET_SYMBOL_VALUE_METHOD_NAME, SymbolStruct.class, LispStruct.class);

	String ENVIRONMENT_GET_FUNCTION_METHOD_NAME = "getFunction";

	String ENVIRONMENT_GET_FUNCTION_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_GET_FUNCTION_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_BIND_FUNCTION_METHOD_NAME = "bindFunction";

	String ENVIRONMENT_BIND_FUNCTION_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_BIND_FUNCTION_METHOD_NAME, SymbolStruct.class, FunctionStruct.class);

	String ENVIRONMENT_UNBIND_FUNCTION_METHOD_NAME = "unbindFunction";

	String ENVIRONMENT_UNBIND_FUNCTION_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_UNBIND_FUNCTION_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_GET_MACRO_FUNCTION_EXPANDER_METHOD_NAME = "getMacroFunctionExpander";

	String ENVIRONMENT_GET_MACRO_FUNCTION_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_GET_MACRO_FUNCTION_EXPANDER_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_BIND_MACRO_FUNCTION_EXPANDER_METHOD_NAME = "bindMacroFunctionExpander";

	String ENVIRONMENT_BIND_MACRO_FUNCTION_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_BIND_MACRO_FUNCTION_EXPANDER_METHOD_NAME, SymbolStruct.class, MacroFunctionExpanderInter.class);

	String ENVIRONMENT_UNBIND_MACRO_FUNCTION_EXPANDER_METHOD_NAME = "unbindMacroFunctionExpander";

	String ENVIRONMENT_UNBIND_MACRO_FUNCTION_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_UNBIND_MACRO_FUNCTION_EXPANDER_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_GET_SYMBOL_MACRO_EXPANDER_METHOD_NAME = "getSymbolMacroExpander";

	String ENVIRONMENT_GET_SYMBOL_MACRO_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_GET_SYMBOL_MACRO_EXPANDER_METHOD_NAME, SymbolStruct.class);

	String ENVIRONMENT_BIND_SYMBOL_MACRO_EXPANDER_METHOD_NAME = "bindSymbolMacroExpander";

	String ENVIRONMENT_BIND_SYMBOL_MACRO_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_BIND_SYMBOL_MACRO_EXPANDER_METHOD_NAME, SymbolStruct.class, SymbolMacroExpanderInter.class);

	String ENVIRONMENT_UNBIND_SYMBOL_MACRO_EXPANDER_METHOD_NAME = "unbindSymbolMacroExpander";

	String ENVIRONMENT_UNBIND_SYMBOL_MACRO_EXPANDER_METHOD_DESC = CodeGenerators.getMethodDescription(Environment.class, ENVIRONMENT_UNBIND_SYMBOL_MACRO_EXPANDER_METHOD_NAME, SymbolStruct.class);

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

	String COMPILED_FUNCTION_STRUCT_INIT_ENVIRONMENT_DESC = CodeGenerators.getConstructorDescription(CompiledFunctionStruct.class, Environment.class);

	String COMPILED_FUNCTION_STRUCT_INIT_STRING_ENVIRONMENT_DESC = CodeGenerators.getConstructorDescription(CompiledFunctionStruct.class, String.class, Environment.class);

	String FUNCTION_STRUCT_APPLY_METHOD_NAME = "apply";

	String FUNCTION_STRUCT_APPLY_METHOD_DESC = CodeGenerators.getMethodDescription(FunctionStruct.class, FUNCTION_STRUCT_APPLY_METHOD_NAME, LispStruct[].class);

	String COMPILED_MACRO_FUNCTION_EXPANDER_NAME = Type.getInternalName(CompiledMacroFunctionExpander.class);

	String COMPILED_MACRO_FUNCTION_EXPANDER_CLASS_SIGNATURE = "Ljcl/compiler/function/expanders/CompiledMacroFunctionExpander<Ljcl/lang/LispStruct;>;";

	String PATHNAME_STRUCT_NAME = Type.getInternalName(PathnameStruct.class);

	String PATHNAME_STRUCT_TO_PATHNAME_METHOD_NAME = "toPathname";

	String PATHNAME_STRUCT_TO_PATHNAME_METHOD_DESC = CodeGenerators.getMethodDescription(PathnameStruct.class, PATHNAME_STRUCT_TO_PATHNAME_METHOD_NAME, LispStruct.class, LispStruct.class, LispStruct.class, LispStruct.class, LispStruct.class, LispStruct.class);

	String LOGICAL_PATHNAME_STRUCT_NAME = Type.getInternalName(LogicalPathnameStruct.class);

	String LOGICAL_PATHNAME_STRUCT_TO_LOGICAL_PATHNAME_METHOD_NAME = "toLogicalPathname";

	String LOGICAL_PATHNAME_STRUCT_TO_LOGICAL_PATHNAME_METHOD_DESC = CodeGenerators.getMethodDescription(LogicalPathnameStruct.class, LOGICAL_PATHNAME_STRUCT_TO_LOGICAL_PATHNAME_METHOD_NAME, LispStruct.class, LispStruct.class, LispStruct.class, LispStruct.class, LispStruct.class);

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
}
