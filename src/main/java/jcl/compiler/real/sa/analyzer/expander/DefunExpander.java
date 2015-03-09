package jcl.compiler.real.sa.analyzer.expander;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class DefunExpander implements MacroFunctionExpander {

	public LispStruct expand(final ListStruct form) {

		final ListStruct allButDefun = form.getRest();
		LispStruct functionName = allButDefun.getFirst();
		final ListStruct lambdaParamsAndBody = allButDefun.getRest();

		// Create and add: (progn (set-symbol[-setf]-function (quote foo) (lambda (x) (block foo x))) (quote foo))
		final List<LispStruct> expandedForm = new ArrayList<>();
		expandedForm.add(SpecialOperator.PROGN);

		final List<LispStruct> setFunction = new ArrayList<>();

		if (functionName instanceof SymbolStruct) {

			final SymbolStruct<?> setSymbolFunctionSymbol = GlobalPackageStruct.COMMON_LISP.intern("SET-SYMBOL-FUNCTION").getSymbol();
			setFunction.add(setSymbolFunctionSymbol);
		} else if (functionName instanceof ListStruct) {

			final ListStruct functionNameList = (ListStruct) functionName;
			if (functionNameList.size() != 2) {
				throw new RuntimeException("Improper function name supplied to DEFUN: " + functionName);
			}

			final LispStruct functionNameListFirst = functionNameList.getFirst();
			final SymbolStruct<?> setfSymbol = GlobalPackageStruct.COMMON_LISP.intern("SETF").getSymbol();

			// (setf foo)
			if (!functionNameListFirst.equals(setfSymbol)) {
				throw new RuntimeException("Improper function name supplied to DEFUN: " + functionName);
			}

			functionName = functionNameList.getRest().getFirst();
			if (!(functionName instanceof SymbolStruct)) {
				throw new RuntimeException("Improper function name supplied to DEFUN: " + functionName);
			}

			// TODO: This is a DEFSETF lambda list, NOT an ORDINARY lambda list!!!
			final SymbolStruct<?> setSymbolSetfFunctionSymbol = GlobalPackageStruct.COMMON_LISP.intern("SET-SYMBOL-SETF-FUNCTION").getSymbol();
			setFunction.add(setSymbolSetfFunctionSymbol);
		} else {
			throw new RuntimeException("Improper function name supplied to DEFUN: " + functionName);
		}

		// Create: (quote foo)
		final List<LispStruct> quoteFunctionName = new ArrayList<>();
		quoteFunctionName.add(SpecialOperator.QUOTE);
		quoteFunctionName.add(functionName);

		final ListStruct quoteFunctionNameList = ListStruct.buildProperList(quoteFunctionName);
		setFunction.add(quoteFunctionNameList);
		// Add: (quote foo)

		// Create: (lambda (x) (block foo x))
		final List<LispStruct> lambdaForm = new ArrayList<>();
		lambdaForm.add(SpecialOperator.LAMBDA);

		// Create: (x)
		final LispStruct lambdaParams = lambdaParamsAndBody.getFirst();
		lambdaForm.add(lambdaParams);
		// Add: (x)

		// Create: (block foo x)
		final List<LispStruct> blockForm = new ArrayList<>();
		blockForm.add(SpecialOperator.BLOCK);
		blockForm.add(functionName);

		final ListStruct lambdaBody = lambdaParamsAndBody.getRest();
		final List<LispStruct> lambdaBodyList = lambdaBody.getAsJavaList();
		blockForm.addAll(lambdaBodyList);

		final ListStruct blockFormList = ListStruct.buildProperList(blockForm);
		lambdaForm.add(blockFormList);
		// Add: (block foo x)

		final ListStruct lambdaFormList = ListStruct.buildProperList(lambdaForm);
		setFunction.add(lambdaFormList);
		// Add: (lambda (x) (block foo x))

		final ListStruct setFunctionList = ListStruct.buildProperList(setFunction);
		expandedForm.add(setFunctionList);
		// Add: (set-symbol[-setf]-function (quote foo) (lambda (x) (block foo x)))

		expandedForm.add(quoteFunctionNameList);
		// Add (to end): (quote foo)

		// Return: (progn (set-symbol[-setf]-function (quote foo) (lambda (x) (block foo x))) (quote foo))
		return ListStruct.buildProperList(expandedForm);
	}

	protected SpecialOperator getLambdaSymbol() {
		return SpecialOperator.LAMBDA;
	}
}
