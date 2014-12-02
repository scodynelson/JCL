package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class QuoteAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private LoadTimeValueAnalyzer loadTimeValueAnalyzer;

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct element = input.getRest().getFirst();

		final ListStruct newForm;
		if (element instanceof ListStruct) {
			newForm = analyzeQuoteList(analyzer, (ListStruct) element, analysisBuilder);
		} else if (element instanceof SymbolStruct) {
			newForm = analyzeQuoteSymbol((SymbolStruct) element);
		} else {
			return element;
		}

		// If was ListStruct or SymbolStruct, wrap resulting form in Load-Time-Value.
		final ListStruct initForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, newForm);
		return loadTimeValueAnalyzer.analyze(analyzer, initForm, analysisBuilder);
	}

	private static ListStruct analyzeQuoteList(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		final SymbolStruct<?> listFnSym;
		if (input.isDotted()) {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST*").getSymbolStruct();
		} else {
			listFnSym = GlobalPackageStruct.COMMON_LISP.findSymbol("LIST").getSymbolStruct();
		}

		final List<LispStruct> formJavaList = input.getAsJavaList();

		final List<LispStruct> transformedForms = formJavaList
				.stream()
				.map(e -> analyzer.analyzeForm(e, analysisBuilder))
				.collect(Collectors.toList());

		final List<LispStruct> transformedListForms = new ArrayList<>();
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(transformedForms);

		return ListStruct.buildProperList(transformedListForms);
	}

	private static ListStruct analyzeQuoteSymbol(final SymbolStruct<?> input) {
		final String symbolFunctionString;
		final String symbolNameString;

		final PackageStruct symbolPackage = input.getSymbolPackage();
		if (symbolPackage != null) {
			symbolFunctionString = "FIND-SYMBOL";
			symbolNameString = symbolPackage.getName();
		} else {
			symbolFunctionString = "MAKE-SYMBOL";
			symbolNameString = input.getName();
		}

		final List<LispStruct> symbolQuoteList = new ArrayList<>();
		symbolQuoteList.add(GlobalPackageStruct.COMMON_LISP.findSymbol(symbolFunctionString).getSymbolStruct());
		symbolQuoteList.add(new StringStruct(symbolNameString));

		return ListStruct.buildProperList(symbolQuoteList);
	}
}
