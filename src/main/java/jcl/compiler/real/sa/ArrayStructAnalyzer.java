package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
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

public class ArrayStructAnalyzer implements Analyzer<LispStruct, ArrayStruct<?>> {

	public static final ArrayStructAnalyzer INSTANCE = new ArrayStructAnalyzer();

	@Override
	public LispStruct analyze(final ArrayStruct<?> input) {
		final ListStruct formList = createSimpleArrayInitialContentsList(input);

		// (system::%make-array)
		ListStruct functionMakeArray = ListStruct.buildProperList(new SymbolStruct("%MAKE-ARRAY", GlobalPackageStruct.SYSTEM));
		// (function system::%make-array)
		functionMakeArray = new ConsStruct(SpecialOperator.FUNCTION, functionMakeArray);

		// ((formList))
		ListStruct finalList = ListStruct.buildProperList(formList);
		// (nil (formList))
		finalList = new ConsStruct(NILStruct.INSTANCE, finalList);
		// (t nil (formList))
		finalList = new ConsStruct(TStruct.INSTANCE, finalList);
		// (((dimensions) t nil (formList)))
		if (input.getRank() == 0) {
			finalList = ListStruct.buildProperList(new ConsStruct(NullStruct.INSTANCE, finalList));
		} else {
//TODO:			finalList = ListStruct.buildProperList(new ConsStruct(ListStruct.buildProperList(input.getDimensions()), finalList));
		}
		// ((quote ((dimensions) t nil (formList))))
		finalList = ListStruct.buildProperList(new ConsStruct(SpecialOperator.QUOTE, finalList));
		// ((function system::%make-array) (quote ((dimensions) t nil (formList))))
		finalList = new ConsStruct(functionMakeArray, finalList);
		// ((apply (function system::%make-array) (quote ((dimensions) t nil (formList)))))
		finalList = ListStruct.buildProperList(new ConsStruct(GlobalPackageStruct.COMMON_LISP.findSymbol("APPLY").getSymbolStruct(), finalList));
		final ListStruct ltvList = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, finalList);
		return SemanticAnalyzer.saMainLoop(ltvList);
	}

	private static ListStruct createSimpleArrayInitialContentsList(final ArrayStruct formArray) {
		ListStruct formList = NullStruct.INSTANCE;

		for (int i = 0; i < formArray.getTotalSize(); i++) {
			formList = new ConsStruct(formArray.getElementAt(i), formList);
		}
		formList = NReverseFunction.funcall(formList);
		if ((formArray.getRank() > 1) && !formList.equals(NullStruct.INSTANCE)) {
			formList = initialContentsBuilder(formList, ListStruct.buildProperList(formArray.getDimensions()));
		}
		return formList;
	}

	private static ListStruct initialContentsBuilder(ListStruct formList, final ListStruct dimensionsList) {
		ListStruct newList = NullStruct.INSTANCE;

		if (dimensionsList.size() > 1) {
			for (int i = 0; i < ((IntegerStruct) dimensionsList.getFirst()).getBigInteger().intValue(); i++) {
				newList = new ConsStruct(initialContentsBuilder(subListFinder(formList, dimensionsList.getRest()), dimensionsList.getRest()), newList);
				formList = reduceListSize(formList, dimensionsList.getRest());
			}
			newList = NReverseFunction.funcall(newList);
		} else {
			newList = formList;
		}
		return newList;
	}

	private static ListStruct subListFinder(ListStruct formList, final ListStruct dimensionsList) {
		ListStruct newList = NullStruct.INSTANCE;

		int sizeOfSubList = 1;
		final Object[] dimensionsAsArray = dimensionsList.getAsJavaList().toArray();
		for (int i = 0; i < dimensionsList.size(); i++) {
			sizeOfSubList *= ((IntegerStruct) dimensionsAsArray[i]).getBigInteger().intValue();
		}
		for (int j = 0; j < sizeOfSubList; j++) {
			newList = new ConsStruct(formList.getFirst(), newList);
			formList = formList.getRest();
		}

		return NReverseFunction.funcall(newList);
	}

	private static ListStruct reduceListSize(ListStruct formList, final ListStruct dimensionsList) {

		int amountToReduce = 1;
		final Object[] dimensionsAsArray = dimensionsList.getAsJavaList().toArray();
		for (int i = 0; i < dimensionsList.size(); i++) {
			amountToReduce *= ((IntegerStruct) dimensionsAsArray[i]).getBigInteger().intValue();
		}
		for (int j = 0; j < amountToReduce; j++) {
			formList = formList.getRest();
		}

		return formList;
	}
}
