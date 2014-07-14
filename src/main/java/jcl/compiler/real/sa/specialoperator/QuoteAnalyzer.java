package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.arrays.StringStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.ArrayStructAnalyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class QuoteAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final QuoteAnalyzer INSTANCE = new QuoteAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		return analyze(input, null);
	}

	public LispStruct analyze(final ListStruct input, final String fieldName) {
		if (input.size() != 2) {
			throw new RuntimeException("Wrong number of arguments to special operatorQUOTE: " + input.size());
		}

		// don't make up a lot of load-time-value stuff for constants
		final LispStruct element = input.getRest().getFirst();
		if ((element instanceof NumberStruct) || (element instanceof CharacterStruct)) {
			return element;
		}

		if ((element instanceof SymbolStruct) && (((SymbolStruct) element).getSymbolPackage() != null)) {
			return input; // handled in icg
		}

		// Now, it's possible that we've seen this element before
		// If so, we've already made up a LOAD-TIME-VALUE form. No
		// need to make another...
		final LispStruct previousElement = SemanticAnalyzer.dupSet.get(element);
		if (previousElement instanceof SymbolStruct) {
			// the SymbolStruct is really the name of an LTV value
			return ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, previousElement);
			// the icg will turn it into a field access instruction
		}

		// The rest of this code creates a LET form (possibly with no args).
		// The LET body consists of code to re-create the original form that
		// was quoted in the source code. Many of the components are made up
		// with load-time-value forms

		//*** NOTE ***
		// For now, the code does NOT handle multiple instances of uninterned
		// SymbolStructs and does not handle circular lists.

		final ListStruct initForm = transformQuoteToLTV(element);
		final ListStruct newList = new ConsStruct(SpecialOperator.LOAD_TIME_VALUE, initForm);

		final String realFldName = (fieldName == null) ? "LOAD_TIME_VALUE_" : fieldName;
		return LoadTimeValueAnalyzer.INSTANCE.analyze(newList, realFldName, GensymFunction.funcall(String.valueOf(System.currentTimeMillis()) + '_').toString());
	}

	private static ListStruct transformQuoteToLTV(final LispStruct form) {
		final ListStruct newForm;
		// dispatch on type
		if (form instanceof ListStruct) {
			newForm = transformListToLTV((ListStruct) form);
		} else if (form instanceof SymbolStruct) {
			newForm = transformSymbolToLTV((SymbolStruct) form);
		} else if (form instanceof RatioStruct) {
			newForm = transformRatioToLTV((RatioStruct) form);
		} else if (form instanceof ComplexStruct) {
			newForm = transformComplexToLTV((ComplexStruct) form);
		} else if (form instanceof VectorStruct) {
			newForm = transformSimpleVectorToLTV((VectorStruct) form);
		} else if (form instanceof ArrayStruct) {
			newForm = (ListStruct) ArrayStructAnalyzer.INSTANCE.analyze((ArrayStruct) form);
		} else {
			throw new RuntimeException("Can't create a load-time-value form for " + form);
		}
		return newForm;
	}

	private static ListStruct transformListToLTV(final ListStruct formList) {
		final SymbolStruct<?> listFnSym;
		if (formList.isDotted()) {
			// gen (list* <forms>)
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST*").getSymbolStruct();
		} else {
			// gen (list <forms>)
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST").getSymbolStruct();
		}

		final List<LispStruct> transformedForms = new ArrayList<>();

		final List<LispStruct> formJavaList = formList.getAsJavaList();
		for (final LispStruct currentForm : formJavaList) {
			final LispStruct transformedForm = SemanticAnalyzer.saMainLoop(currentForm);
			transformedForms.add(transformedForm);
		}

		final List<LispStruct> transformedListForms = new ArrayList<>();
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedListForms);
	}

	private static ListStruct transformSymbolToLTV(final SymbolStruct<?> SymbolStruct) {
		if (SymbolStruct.getSymbolPackage() != null) {
			// gen (intern "sym name" (find-package "pkg name"))
			final List<LispStruct> symbolFindSymbolPattern = new ArrayList<>();
			symbolFindSymbolPattern.add(GlobalPackageStruct.COMMON_LISP.findSymbol("FIND-PACKAGE").getSymbolStruct());
			symbolFindSymbolPattern.add(new StringStruct(SymbolStruct.getSymbolPackage().getName()));
			final ListStruct symbolFindSymbolPatternList = ListStruct.buildProperList(symbolFindSymbolPattern);

			final List<LispStruct> symbolInternPattern = new ArrayList<>();
			symbolInternPattern.add(GlobalPackageStruct.COMMON_LISP.findSymbol("INTERN").getSymbolStruct());
			symbolInternPattern.add(new StringStruct(SymbolStruct.getName()));
			symbolInternPattern.add(symbolFindSymbolPatternList);

			return ListStruct.buildProperList(symbolInternPattern);
		} else {
			// gen (make-symbol "sym name")
			final List<LispStruct> symbolMakeSymbolPattern = new ArrayList<>();
			symbolMakeSymbolPattern.add(GlobalPackageStruct.COMMON_LISP.findSymbol("MAKE-SYMBOL").getSymbolStruct());
			symbolMakeSymbolPattern.add(new StringStruct(SymbolStruct.getName()));
			final ListStruct symbolMakeSymbolPatternList = ListStruct.buildProperList(symbolMakeSymbolPattern);
			return ListStruct.buildProperList(symbolMakeSymbolPatternList);
		}
	}

	private static ListStruct transformRatioToLTV(final RatioStruct ratio) {
		// gen (/ numerator denominator)
		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("/").getSymbolStruct(), new IntegerStruct(ratio.getBigFraction().getNumerator()), new IntegerStruct(ratio.getBigFraction().getDenominator()));
	}

	private static ListStruct transformComplexToLTV(final ComplexStruct complex) {
		// gen (complex real imaginary)
		return ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP.findSymbol("COMPLEX").getSymbolStruct(), complex.getReal(), complex.getImaginary());
	}

	private static ListStruct transformSimpleVectorToLTV(final VectorStruct<LispStruct> formVector) {
		final SymbolStruct<?> vectorFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("VECTOR").getSymbolStruct();

		final List<LispStruct> transformedForms = new ArrayList<>();

		final List<LispStruct> formJavaList = formVector.getContents();
		for (final LispStruct currentForm : formJavaList) {
			final LispStruct transformedForm = SemanticAnalyzer.saMainLoop(currentForm);
			transformedForms.add(transformedForm);
		}

		final List<LispStruct> transformedVectorForms = new ArrayList<>();
		transformedVectorForms.add(vectorFnSym);
		transformedVectorForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedVectorForms);
	}
}
