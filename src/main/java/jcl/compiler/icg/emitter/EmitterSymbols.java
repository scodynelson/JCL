package jcl.compiler.icg.emitter;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;

/**
 * The {@link EmitterSymbols} is the global location for system defined symbols for the 'EMIT' package.
 */
@SuppressWarnings("all")
public interface EmitterSymbols {

	/*
	OpCodes
	 */

	SymbolStruct NOP = internAndExportSymbol("NOP");

	SymbolStruct ACONST_NULL = internAndExportSymbol("ACONST_NULL");
	SymbolStruct ICONST = internAndExportSymbol("ICONST");
	SymbolStruct LCONST = internAndExportSymbol("LCONST");
	SymbolStruct FCONST = internAndExportSymbol("FCONST");
	SymbolStruct DCONST = internAndExportSymbol("DCONST");

	SymbolStruct BIPUSH = internAndExportSymbol("BIPUSH");
	SymbolStruct SIPUSH = internAndExportSymbol("SIPUSH");

	SymbolStruct LDC = internAndExportSymbol("LDC");

	SymbolStruct ILOAD = internAndExportSymbol("ILOAD");
	SymbolStruct LLOAD = internAndExportSymbol("LLOAD");
	SymbolStruct FLOAD = internAndExportSymbol("FLOAD");
	SymbolStruct DLOAD = internAndExportSymbol("DLOAD");
	SymbolStruct ALOAD = internAndExportSymbol("ALOAD");

	SymbolStruct IALOAD = internAndExportSymbol("IALOAD");
	SymbolStruct LALOAD = internAndExportSymbol("LALOAD");
	SymbolStruct FALOAD = internAndExportSymbol("FALOAD");
	SymbolStruct DALOAD = internAndExportSymbol("DALOAD");
	SymbolStruct AALOAD = internAndExportSymbol("AALOAD");
	SymbolStruct BALOAD = internAndExportSymbol("BALOAD");
	SymbolStruct CALOAD = internAndExportSymbol("CALOAD");
	SymbolStruct SALOAD = internAndExportSymbol("SALOAD");

	SymbolStruct ISTORE = internAndExportSymbol("ISTORE");
	SymbolStruct LSTORE = internAndExportSymbol("LSTORE");
	SymbolStruct FSTORE = internAndExportSymbol("FSTORE");
	SymbolStruct DSTORE = internAndExportSymbol("DSTORE");
	SymbolStruct ASTORE = internAndExportSymbol("ASTORE");

	SymbolStruct IASTORE = internAndExportSymbol("IASTORE");
	SymbolStruct LASTORE = internAndExportSymbol("LASTORE");
	SymbolStruct FASTORE = internAndExportSymbol("FASTORE");
	SymbolStruct DASTORE = internAndExportSymbol("DASTORE");
	SymbolStruct AASTORE = internAndExportSymbol("AASTORE");
	SymbolStruct BASTORE = internAndExportSymbol("BASTORE");
	SymbolStruct CASTORE = internAndExportSymbol("CASTORE");
	SymbolStruct SASTORE = internAndExportSymbol("SASTORE");

	SymbolStruct POP = internAndExportSymbol("POP");
	SymbolStruct POP2 = internAndExportSymbol("POP2");
	SymbolStruct DUP = internAndExportSymbol("DUP");
	SymbolStruct DUP_X1 = internAndExportSymbol("DUP_X1");
	SymbolStruct DUP_X2 = internAndExportSymbol("DUP_X2");
	SymbolStruct DUP2 = internAndExportSymbol("DUP2");
	SymbolStruct DUP2_X1 = internAndExportSymbol("DUP2_X1");
	SymbolStruct DUP2_X2 = internAndExportSymbol("DUP2_X2");
	SymbolStruct SWAP = internAndExportSymbol("SWAP");

	SymbolStruct IADD = internAndExportSymbol("IADD");
	SymbolStruct LADD = internAndExportSymbol("LADD");
	SymbolStruct FADD = internAndExportSymbol("FADD");
	SymbolStruct DADD = internAndExportSymbol("DADD");

	SymbolStruct ISUB = internAndExportSymbol("ISUB");
	SymbolStruct LSUB = internAndExportSymbol("LSUB");
	SymbolStruct FSUB = internAndExportSymbol("FSUB");
	SymbolStruct DSUB = internAndExportSymbol("DSUB");

	SymbolStruct IMUL = internAndExportSymbol("IMUL");
	SymbolStruct LMUL = internAndExportSymbol("LMUL");
	SymbolStruct FMUL = internAndExportSymbol("FMUL");
	SymbolStruct DMUL = internAndExportSymbol("DMUL");

	SymbolStruct IDIV = internAndExportSymbol("IDIV");
	SymbolStruct LDIV = internAndExportSymbol("LDIV");
	SymbolStruct FDIV = internAndExportSymbol("FDIV");
	SymbolStruct DDIV = internAndExportSymbol("DDIV");

	SymbolStruct IREM = internAndExportSymbol("IREM");
	SymbolStruct LREM = internAndExportSymbol("LREM");
	SymbolStruct FREM = internAndExportSymbol("FREM");
	SymbolStruct DREM = internAndExportSymbol("DREM");

	SymbolStruct INEG = internAndExportSymbol("INEG");
	SymbolStruct LNEG = internAndExportSymbol("LNEG");
	SymbolStruct FNEG = internAndExportSymbol("FNEG");
	SymbolStruct DNEG = internAndExportSymbol("DNEG");

	SymbolStruct ISHL = internAndExportSymbol("ISHL");
	SymbolStruct LSHL = internAndExportSymbol("LSHL");
	SymbolStruct ISHR = internAndExportSymbol("ISHR");
	SymbolStruct LSHR = internAndExportSymbol("LSHR");
	SymbolStruct IUSHR = internAndExportSymbol("IUSHR");
	SymbolStruct LUSHR = internAndExportSymbol("LUSHR");

	SymbolStruct IAND = internAndExportSymbol("IAND");
	SymbolStruct LAND = internAndExportSymbol("LAND");
	SymbolStruct IOR = internAndExportSymbol("IOR");
	SymbolStruct LOR = internAndExportSymbol("LOR");
	SymbolStruct IXOR = internAndExportSymbol("IXOR");
	SymbolStruct LXOR = internAndExportSymbol("LXOR");

	SymbolStruct IINC = internAndExportSymbol("IINC");

	SymbolStruct I2L = internAndExportSymbol("I2L");
	SymbolStruct I2F = internAndExportSymbol("I2F");
	SymbolStruct I2D = internAndExportSymbol("I2D");

	SymbolStruct L2I = internAndExportSymbol("L2I");
	SymbolStruct L2F = internAndExportSymbol("L2F");
	SymbolStruct L2D = internAndExportSymbol("L2D");

	SymbolStruct F2I = internAndExportSymbol("F2I");
	SymbolStruct F2L = internAndExportSymbol("F2L");
	SymbolStruct F2D = internAndExportSymbol("F2D");

	SymbolStruct D2I = internAndExportSymbol("D2I");
	SymbolStruct D2L = internAndExportSymbol("D2L");
	SymbolStruct D2F = internAndExportSymbol("D2F");

	SymbolStruct I2B = internAndExportSymbol("I2B");
	SymbolStruct I2C = internAndExportSymbol("I2C");
	SymbolStruct I2S = internAndExportSymbol("I2S");

	SymbolStruct LCMP = internAndExportSymbol("LCMP");
	SymbolStruct FCMPL = internAndExportSymbol("FCMPL");
	SymbolStruct FCMPG = internAndExportSymbol("FCMPG");
	SymbolStruct DCMPL = internAndExportSymbol("DCMPL");
	SymbolStruct DCMPG = internAndExportSymbol("DCMPG");

	SymbolStruct IFEQ = internAndExportSymbol("IFEQ");
	SymbolStruct IFNE = internAndExportSymbol("IFNE");
	SymbolStruct IFLT = internAndExportSymbol("IFLT");
	SymbolStruct IFGE = internAndExportSymbol("IFGE");
	SymbolStruct IFGT = internAndExportSymbol("IFGT");
	SymbolStruct IFLE = internAndExportSymbol("IFLE");

	SymbolStruct IF_ICMPEQ = internAndExportSymbol("IF_ICMPEQ");
	SymbolStruct IF_ICMPNE = internAndExportSymbol("IF_ICMPNE");
	SymbolStruct IF_ICMPLT = internAndExportSymbol("IF_ICMPLT");
	SymbolStruct IF_ICMPGE = internAndExportSymbol("IF_ICMPGE");
	SymbolStruct IF_ICMPGT = internAndExportSymbol("IF_ICMPGT");
	SymbolStruct IF_ICMPLE = internAndExportSymbol("IF_ICMPLE");

	SymbolStruct IF_ACMPEQ = internAndExportSymbol("IF_ACMPEQ");
	SymbolStruct IF_ACMPNE = internAndExportSymbol("IF_ACMPNE");

	SymbolStruct GOTO = internAndExportSymbol("GOTO");
	SymbolStruct JSR = internAndExportSymbol("JSR");
	SymbolStruct RET = internAndExportSymbol("RET");
	SymbolStruct TABLESWITCH = internAndExportSymbol("TABLESWITCH");
	SymbolStruct LOOKUPSWITCH = internAndExportSymbol("LOOKUPSWITCH");

	SymbolStruct IRETURN = internAndExportSymbol("IRETURN");
	SymbolStruct LRETURN = internAndExportSymbol("LRETURN");
	SymbolStruct FRETURN = internAndExportSymbol("FRETURN");
	SymbolStruct DRETURN = internAndExportSymbol("DRETURN");
	SymbolStruct ARETURN = internAndExportSymbol("ARETURN");
	SymbolStruct RETURN = internAndExportSymbol("RETURN");

	SymbolStruct GETSTATIC = internAndExportSymbol("GETSTATIC");
	SymbolStruct PUTSTATIC = internAndExportSymbol("PUTSTATIC");
	SymbolStruct GETFIELD = internAndExportSymbol("GETFIELD");
	SymbolStruct PUTFIELD = internAndExportSymbol("PUTFIELD");

	SymbolStruct INVOKEVIRTUAL = internAndExportSymbol("INVOKEVIRTUAL");
	SymbolStruct INVOKESPECIAL = internAndExportSymbol("INVOKESPECIAL");
	SymbolStruct INVOKESTATIC = internAndExportSymbol("INVOKESTATIC");
	SymbolStruct INVOKEINTERFACE = internAndExportSymbol("INVOKEINTERFACE");
	SymbolStruct INVOKEDYNAMIC = internAndExportSymbol("INVOKEDYNAMIC");

	SymbolStruct NEW = internAndExportSymbol("NEW");
	SymbolStruct NEWARRAY = internAndExportSymbol("NEWARRAY");
	SymbolStruct ANEWARRAY = internAndExportSymbol("ANEWARRAY");

	SymbolStruct ARRAYLENGTH = internAndExportSymbol("ARRAYLENGTH");

	SymbolStruct ATHROW = internAndExportSymbol("ATHROW");

	SymbolStruct CHECKCAST = internAndExportSymbol("CHECKCAST");
	SymbolStruct INSTANCEOF = internAndExportSymbol("INSTANCEOF");

	SymbolStruct MONITORENTER = internAndExportSymbol("MONITORENTER");
	SymbolStruct MONITOREXIT = internAndExportSymbol("MONITOREXIT");

	SymbolStruct MULTIANEWARRAY = internAndExportSymbol("MULTIANEWARRAY");

	SymbolStruct IFNULL = internAndExportSymbol("IFNULL");
	SymbolStruct IFNONNULL = internAndExportSymbol("IFNONNULL");

	/*
	Visitors
	 */

	SymbolStruct EMIT_ANNOTATION_FIELD = internAndExportSymbol("EMIT-ANNOTATION-FIELD");
	SymbolStruct EMIT_FIELD = internAndExportSymbol("EMIT-FIELD");
	SymbolStruct EMIT_LABEL = internAndExportSymbol("EMIT-LABEL");
	SymbolStruct EMIT_LINE = internAndExportSymbol("EMIT-LINE");
	SymbolStruct EMIT_SOURCE_FILE = internAndExportSymbol("EMIT-SOURCE-FILE");
	SymbolStruct EMIT_TRY_CATCH_BLOCK = internAndExportSymbol("EMIT-TRY-CATCH-BLOCK");

	/*
	Creators
	 */

	SymbolStruct MAKE_CONSTANT_DYNAMIC = internAndExportSymbol("MAKE-CONSTANT-DYNAMIC");
	SymbolStruct MAKE_HANDLE = internAndExportSymbol("MAKE-HANDLE");
	SymbolStruct MAKE_LABEL = internAndExportSymbol("MAKE-LABEL");

	/*
	Start/End
	 */

	SymbolStruct NEW_CLASS_ANNOTATION = internAndExportSymbol("NEW-CLASS-ANNOTATION");
	SymbolStruct NEW_FIELD_ANNOTATION = internAndExportSymbol("NEW-FIELD-ANNOTATION");
	SymbolStruct NEW_METHOD_ANNOTATION = internAndExportSymbol("NEW-METHOD-ANNOTATION");
	SymbolStruct NEW_CLASS = internAndExportSymbol("NEW-CLASS");
	SymbolStruct NEW_FIELD = internAndExportSymbol("NEW-FIELD");
	SymbolStruct NEW_METHOD = internAndExportSymbol("NEW-METHOD");

	SymbolStruct ADD_INNER_CLASS_REF = internAndExportSymbol("ADD-INNER-CLASS-REF");
	SymbolStruct ADD_OUTER_CLASS_REF = internAndExportSymbol("ADD-OUTER-CLASS-REF");

	SymbolStruct END_ANNOTATION = internAndExportSymbol("END-ANNOTATION");
	SymbolStruct END_CLASS = internAndExportSymbol("END-CLASS");
	SymbolStruct END_FIELD = internAndExportSymbol("END-FIELD");
	SymbolStruct END_METHOD = internAndExportSymbol("END-METHOD");

	/*
	Emitters
	 */

	SymbolStruct EMIT_NIL = internAndExportSymbol("EMIT-NIL");
	SymbolStruct EMIT_T = internAndExportSymbol("EMIT-T");
	SymbolStruct EMIT_SYMBOL_PACKAGE = internAndExportSymbol("EMIT-SYMBOL-PACKAGE");
	SymbolStruct EMIT_PACKAGE = internAndExportSymbol("EMIT-PACKAGE");

	/*
	Other
	 */

	SymbolStruct LINE = internAndExportSymbol("LINE");
	SymbolStruct SOURCE = internAndExportSymbol("SOURCE");

	private static SymbolStruct internAndExportSymbol(final String name) {
		final SymbolStruct symbol = GlobalPackageStruct.EMIT.intern(name).getSymbol();
		GlobalPackageStruct.EMIT.export(symbol);
		return symbol;
	}
}