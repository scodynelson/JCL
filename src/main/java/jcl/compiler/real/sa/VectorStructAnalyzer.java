package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.old.functions.NReverseFunction;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;

import java.math.BigInteger;

public class VectorStructAnalyzer implements Analyzer<LispStruct, VectorStruct<?>> {

	public static final VectorStructAnalyzer INSTANCE = new VectorStructAnalyzer();

	@Override
	public LispStruct analyze(final VectorStruct<?> input) {
		ListStruct formList = NullStruct.INSTANCE;
		for (int i = 0; i < input.getTotalSize(); i++) {
			formList = new ConsStruct(input.getElementAt(i), formList);
		}
		formList = NReverseFunction.funcall(formList);

		final LispStruct adjustableBoolean;
		if (input.isAdjustable()) {
			adjustableBoolean = TStruct.INSTANCE;
		} else {
			adjustableBoolean = NILStruct.INSTANCE;
		}

		// (system::%make-array)
		ListStruct functionMakeArray = ListStruct.buildProperList(new SymbolStruct("%MAKE-ARRAY", GlobalPackageStruct.SYSTEM));
		// (function system::%make-array)
		functionMakeArray = new ConsStruct(SpecialOperator.FUNCTION, functionMakeArray);

		// ()
		ListStruct finalList = NullStruct.INSTANCE;
		// (fillPointerIndex)
		if (input.getFillPointer() != null) {
			finalList = new ConsStruct(new IntegerStruct(BigInteger.valueOf(input.getFillPointer())), finalList);
		}
		// ((formList) fillPointerIndex)
		finalList = new ConsStruct(adjustableBoolean, finalList);
		// ((formList))
		finalList = new ConsStruct(formList, finalList);
		// (nil (formList))
		finalList = new ConsStruct(NILStruct.INSTANCE, finalList);
		// (t nil (formList))
		finalList = new ConsStruct(TStruct.INSTANCE, finalList);
		// (((dimensions) t nil (formList)))
//TODO		finalList = ListStruct.buildProperList(new ConsStruct(ListStruct.buildProperList(input.getDimensions()), finalList));
		// ((quote ((dimensions) t nil (formList))))
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperator.QUOTE, finalList));
		// ((function system::%make-array) (quote ((dimensions) t nil (formList))))
		finalList = new ConsStruct(functionMakeArray, finalList);
		// ((apply (function system::%make-array) (quote ((dimensions) t nil (formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.findSymbol("APPLY").getSymbolStruct(), finalList));
		final ListStruct ltvList = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, finalList);
		return SemanticAnalyzer.saMainLoop(ltvList);
	}
}
