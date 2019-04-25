package jcl.lang.classes;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link BuiltInClassStruct} is the object representation of a Lisp 'built-in-class' type.
 */
public final class BuiltInClassStruct extends ClassStruct {

	public static final BuiltInClassStruct CLASS_T = addClass(CommonLispSymbols.T);

	public static final BuiltInClassStruct ARRAY = addClass(CommonLispSymbols.ARRAY);
	public static final BuiltInClassStruct BIGNUM = addClass(CommonLispSymbols.BIGNUM);
	public static final BuiltInClassStruct BASE_STRING = addClass(CommonLispSymbols.BASE_STRING);
	public static final BuiltInClassStruct BIT_VECTOR = addClass(CommonLispSymbols.BIT_VECTOR);
	public static final BuiltInClassStruct BROADCAST_STREAM = addClass(CommonLispSymbols.BROADCAST_STREAM);
	public static final BuiltInClassStruct CHARACTER = addClass(CommonLispSymbols.CHARACTER);
	public static final BuiltInClassStruct COMPLEX = addClass(CommonLispSymbols.COMPLEX);
	public static final BuiltInClassStruct CONS = addClass(CommonLispSymbols.CONS);
	public static final BuiltInClassStruct CONCATENATED_STREAM = addClass(CommonLispSymbols.CONCATENATED_STREAM);
	public static final BuiltInClassStruct DOUBLE_FLOAT = addClass(CommonLispSymbols.DOUBLE_FLOAT);
	public static final BuiltInClassStruct ECHO_STREAM = addClass(CommonLispSymbols.ECHO_STREAM);
	//	public static final BuiltInClassStruct ENVIRONMENT          = addClass(CommonLispSymbols.ENVIRONMENT);
	public static final BuiltInClassStruct FILE_STREAM = addClass(CommonLispSymbols.FILE_STREAM);
	public static final BuiltInClassStruct FIXNUM = addClass(CommonLispSymbols.FIXNUM);
	public static final BuiltInClassStruct FLOAT = addClass(CommonLispSymbols.FLOAT);
	public static final BuiltInClassStruct FUNCTION = addClass(CommonLispSymbols.FUNCTION);
	public static final BuiltInClassStruct HASH_TABLE = addClass(CommonLispSymbols.HASH_TABLE);
	public static final BuiltInClassStruct INTEGER = addClass(CommonLispSymbols.INTEGER);
	//	public static final BuiltInClassStruct JAVA_OBJECT          = addClass(CommonLispSymbols.JAVA_OBJECT);
	public static final BuiltInClassStruct LIST = addClass(CommonLispSymbols.LIST);
	public static final BuiltInClassStruct LOGICAL_PATHNAME = addClass(CommonLispSymbols.LOGICAL_PATHNAME);
	public static final BuiltInClassStruct LONG_FLOAT = addClass(CommonLispSymbols.LONG_FLOAT); // TODO: Check
	public static final BuiltInClassStruct NULL = addClass(CommonLispSymbols.NULL);
	public static final BuiltInClassStruct NUMBER = addClass(CommonLispSymbols.NUMBER);
	public static final BuiltInClassStruct PACKAGE = addClass(CommonLispSymbols.PACKAGE);
	public static final BuiltInClassStruct PATHNAME = addClass(CommonLispSymbols.PATHNAME);
	public static final BuiltInClassStruct RANDOM_STATE = addClass(CommonLispSymbols.RANDOM_STATE);
	public static final BuiltInClassStruct RATIO = addClass(CommonLispSymbols.RATIO);
	public static final BuiltInClassStruct RATIONAL = addClass(CommonLispSymbols.RATIONAL);
	public static final BuiltInClassStruct READTABLE = addClass(CommonLispSymbols.READTABLE);
	public static final BuiltInClassStruct REAL = addClass(CommonLispSymbols.REAL);
	public static final BuiltInClassStruct RESTART = addClass(CommonLispSymbols.RESTART);
	public static final BuiltInClassStruct SEQUENCE = addClass(CommonLispSymbols.SEQUENCE);
	public static final BuiltInClassStruct SHORT_FLOAT = addClass(CommonLispSymbols.SHORT_FLOAT); // TODO: Check
	public static final BuiltInClassStruct SIMPLE_ARRAY = addClass(CommonLispSymbols.SIMPLE_ARRAY);
	public static final BuiltInClassStruct SIMPLE_BASE_STRING = addClass(CommonLispSymbols.SIMPLE_BASE_STRING);
	public static final BuiltInClassStruct SIMPLE_BIT_VECTOR = addClass(CommonLispSymbols.SIMPLE_BIT_VECTOR);
	public static final BuiltInClassStruct SIMPLE_STRING = addClass(CommonLispSymbols.SIMPLE_STRING);
	public static final BuiltInClassStruct SIMPLE_VECTOR = addClass(CommonLispSymbols.SIMPLE_VECTOR);
	public static final BuiltInClassStruct SINGLE_FLOAT = addClass(CommonLispSymbols.SINGLE_FLOAT);
	public static final BuiltInClassStruct STREAM = addClass(CommonLispSymbols.STREAM);
	public static final BuiltInClassStruct STRING = addClass(CommonLispSymbols.STRING);
	//	public static final BuiltInClassStruct STRING_INPUT_STREAM  = addClass(CommonLispSymbols.STRING_INPUT_STREAM);
//	public static final BuiltInClassStruct STRING_OUTPUT_STREAM = addClass(CommonLispSymbols.STRING_OUTPUT_STREAM);
	public static final BuiltInClassStruct STRING_STREAM = addClass(CommonLispSymbols.STRING_STREAM);
	public static final BuiltInClassStruct SYMBOL = addClass(CommonLispSymbols.SYMBOL);
	public static final BuiltInClassStruct SYNONYM_STREAM = addClass(CommonLispSymbols.SYNONYM_STREAM);
	public static final BuiltInClassStruct TWO_WAY_STREAM = addClass(CommonLispSymbols.TWO_WAY_STREAM);
	public static final BuiltInClassStruct VECTOR = addClass(CommonLispSymbols.VECTOR);

	public static final BuiltInClassStruct STRUCTURE_OBJECT = addClass(CommonLispSymbols.STRUCTURE_OBJECT);
//	public static final StructureClassStruct STRUCTURE_OBJECT   = addClass(CommonLispSymbols.STRUCTURE_OBJECT);

	static {
		ARRAY.setDirectSuperClasses(CLASS_T);
		ARRAY.setClassPrecedenceList(ARRAY, CLASS_T);
		BASE_STRING.setDirectSuperClasses(STRING);
		BASE_STRING.setClassPrecedenceList(BASE_STRING, STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
		BIGNUM.setDirectSuperClasses(INTEGER);
		BIGNUM.setClassPrecedenceList(BIGNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
		BIT_VECTOR.setDirectSuperClasses(VECTOR);
		BIT_VECTOR.setClassPrecedenceList(BIT_VECTOR, VECTOR, ARRAY, SEQUENCE, CLASS_T);
		BROADCAST_STREAM.setClassPrecedenceList(BROADCAST_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		CHARACTER.setDirectSuperClasses(CLASS_T);
		CHARACTER.setClassPrecedenceList(CHARACTER, CLASS_T);
		CLASS_T.setClassPrecedenceList(CLASS_T);
		COMPLEX.setDirectSuperClasses(NUMBER);
		COMPLEX.setClassPrecedenceList(COMPLEX, NUMBER, CLASS_T);
		CONCATENATED_STREAM.setClassPrecedenceList(CONCATENATED_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		CONS.setDirectSuperClasses(LIST);
		CONS.setClassPrecedenceList(CONS, LIST, SEQUENCE, CLASS_T);
		DOUBLE_FLOAT.setDirectSuperClasses(FLOAT);
		DOUBLE_FLOAT.setClassPrecedenceList(DOUBLE_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
		ECHO_STREAM.setClassPrecedenceList(ECHO_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
//		ENVIRONMENT.setDirectSuperClasses(CLASS_T);
//		ENVIRONMENT.setClassPrecedenceList(ENVIRONMENT, CLASS_T);
		FIXNUM.setDirectSuperClasses(INTEGER);
		FIXNUM.setClassPrecedenceList(FIXNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
		FILE_STREAM.setClassPrecedenceList(FILE_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		FLOAT.setDirectSuperClasses(REAL);
		FLOAT.setClassPrecedenceList(FLOAT, REAL, NUMBER, CLASS_T);
		FUNCTION.setDirectSuperClasses(CLASS_T);
		FUNCTION.setClassPrecedenceList(FUNCTION, CLASS_T);
		HASH_TABLE.setDirectSuperClasses(CLASS_T);
		HASH_TABLE.setClassPrecedenceList(HASH_TABLE, CLASS_T);
		INTEGER.setDirectSuperClasses(RATIONAL);
		INTEGER.setClassPrecedenceList(INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
//		JAVA_OBJECT.setDirectSuperClasses(CLASS_T);
//		JAVA_OBJECT.setClassPrecedenceList(JAVA_OBJECT, CLASS_T);
		LIST.setDirectSuperClasses(SEQUENCE);
		LIST.setClassPrecedenceList(LIST, SEQUENCE, CLASS_T);
		LOGICAL_PATHNAME.setDirectSuperClasses(PATHNAME);
		LOGICAL_PATHNAME.setClassPrecedenceList(LOGICAL_PATHNAME, PATHNAME, CLASS_T);
		LONG_FLOAT.setDirectSuperClasses(FLOAT);
		LONG_FLOAT.setClassPrecedenceList(LONG_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
		NULL.setDirectSuperClasses(LIST);
		NULL.setClassPrecedenceList(NULL, SYMBOL, LIST, SEQUENCE, CLASS_T);
		NUMBER.setDirectSuperClasses(CLASS_T);
		NUMBER.setClassPrecedenceList(NUMBER, CLASS_T);
		PACKAGE.setDirectSuperClasses(CLASS_T);
		PACKAGE.setClassPrecedenceList(PACKAGE, CLASS_T);
		PATHNAME.setDirectSuperClasses(CLASS_T);
		PATHNAME.setClassPrecedenceList(PATHNAME, CLASS_T);
		RANDOM_STATE.setDirectSuperClasses(CLASS_T);
		RANDOM_STATE.setClassPrecedenceList(RANDOM_STATE, CLASS_T);
		RATIO.setDirectSuperClasses(RATIONAL);
		RATIO.setClassPrecedenceList(RATIO, RATIONAL, REAL, NUMBER, CLASS_T);
		RATIONAL.setDirectSuperClasses(REAL);
		RATIONAL.setClassPrecedenceList(RATIONAL, REAL, NUMBER, CLASS_T);
		READTABLE.setDirectSuperClasses(CLASS_T);
		READTABLE.setClassPrecedenceList(READTABLE, CLASS_T);
		REAL.setDirectSuperClasses(NUMBER);
		REAL.setClassPrecedenceList(REAL, NUMBER, CLASS_T);
		RESTART.setDirectSuperClasses(CLASS_T);
		RESTART.setClassPrecedenceList(RESTART, CLASS_T);
		SEQUENCE.setDirectSuperClasses(CLASS_T);
		SEQUENCE.setClassPrecedenceList(SEQUENCE, CLASS_T);
		SHORT_FLOAT.setDirectSuperClasses(FLOAT);
		SHORT_FLOAT.setClassPrecedenceList(SHORT_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
		SIMPLE_ARRAY.setDirectSuperClasses(ARRAY);
		SIMPLE_ARRAY.setClassPrecedenceList(SIMPLE_ARRAY, ARRAY, CLASS_T);
		SIMPLE_BASE_STRING.setDirectSuperClasses(BASE_STRING, SIMPLE_STRING);
		SIMPLE_BASE_STRING.setClassPrecedenceList(SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING, STRING, VECTOR,
		                                          SIMPLE_ARRAY, ARRAY, SEQUENCE, CLASS_T);
		SIMPLE_BIT_VECTOR.setDirectSuperClasses(BIT_VECTOR, SIMPLE_ARRAY);
		SIMPLE_BIT_VECTOR.setClassPrecedenceList(SIMPLE_BIT_VECTOR, BIT_VECTOR, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE,
		                                         CLASS_T);
		SIMPLE_STRING.setDirectSuperClasses(BASE_STRING, STRING, SIMPLE_ARRAY);
		SIMPLE_STRING.setClassPrecedenceList(SIMPLE_STRING, BASE_STRING, STRING, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE,
		                                     CLASS_T);
		SIMPLE_VECTOR.setDirectSuperClasses(VECTOR, SIMPLE_ARRAY);
		SIMPLE_VECTOR.setClassPrecedenceList(SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY, ARRAY, SEQUENCE, CLASS_T);
		SINGLE_FLOAT.setDirectSuperClasses(FLOAT);
		SINGLE_FLOAT.setClassPrecedenceList(SINGLE_FLOAT, FLOAT, REAL, NUMBER, CLASS_T);
		STREAM.setClassPrecedenceList(STREAM, STRUCTURE_OBJECT, CLASS_T);
		STRING.setDirectSuperClasses(VECTOR);
		STRING.setClassPrecedenceList(STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
//		STRING_INPUT_STREAM.setClassPrecedenceList(STRING_INPUT_STREAM, STRING_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
//		STRING_OUTPUT_STREAM.setClassPrecedenceList(STRING_OUTPUT_STREAM, STRING_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		STRING_STREAM.setClassPrecedenceList(STRING_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		STRUCTURE_OBJECT.setClassPrecedenceList(STRUCTURE_OBJECT, CLASS_T);
		SYMBOL.setDirectSuperClasses(CLASS_T);
		SYMBOL.setClassPrecedenceList(SYMBOL, CLASS_T);
		SYNONYM_STREAM.setClassPrecedenceList(SYNONYM_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		TWO_WAY_STREAM.setClassPrecedenceList(TWO_WAY_STREAM, STREAM, STRUCTURE_OBJECT, CLASS_T);
		VECTOR.setDirectSuperClasses(ARRAY, SEQUENCE);
		VECTOR.setClassPrecedenceList(VECTOR, ARRAY, SEQUENCE, CLASS_T);
	}

	private BuiltInClassStruct(final SymbolStruct name) {
		super(name);
	}

	private static BuiltInClassStruct addClass(final SymbolStruct name) {
		final BuiltInClassStruct classStruct = new BuiltInClassStruct(name);
//		addClass(name, classStruct); // TODO: cannot use this statically due to other static dependencies.
		return classStruct;
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.BUILT_IN_CLASS;
	}

	@Override
	public ClassStruct classOf() {
		return StandardClassStruct.BUILT_IN_CLASS;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.BUILT_IN_CLASS) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == StandardClassStruct.BUILT_IN_CLASS) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
