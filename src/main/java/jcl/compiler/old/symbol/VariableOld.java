package jcl.compiler.old.symbol;

import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;

import java.math.BigInteger;

public class VariableOld {

	public static final SymbolStruct Compile = new SymbolStruct("COMPILE", GlobalPackageStruct.COMMON_LISP);
	public static final SymbolStruct Load = new SymbolStruct("LOAD", GlobalPackageStruct.COMMON_LISP);
	public static final SymbolStruct Eval = new SymbolStruct("EVAL", GlobalPackageStruct.COMMON_LISP);

	public static final SymbolStruct<IntegerStruct> GensymCounter = new SymbolStruct<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.ZERO), null);
	public static final SymbolStruct PrintCircle = new SymbolStruct("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE, null);
	public static final SymbolStruct<FunctionStruct> MacroexpandHook = new SymbolStruct<>("*MACROEXPAND-HOOK*", GlobalPackageStruct.COMMON_LISP, null, null);
	public static final SymbolStruct CompileTrace = new SymbolStruct("*COMPILE-TRACE*", GlobalPackageStruct.COMPILER, null, null);
	public static final SymbolStruct<ListStruct> DocumentationDom = new SymbolStruct<>("*DOCUMENTATION-DOM*", GlobalPackageStruct.SYSTEM, NullStruct.INSTANCE, null);
}
