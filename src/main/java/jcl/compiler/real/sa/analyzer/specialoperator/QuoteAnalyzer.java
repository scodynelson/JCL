package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.StringElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.QuoteElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class QuoteAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2741011595927247743L;

	@Autowired
	private LoadTimeValueAnalyzer loadTimeValueAnalyzer;

	@Override
	public QuoteElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement quotedObject = inputRest.getFirst();

		ConsElement analyzedElement = null;
		if (quotedObject instanceof SymbolElement) {
			analyzedElement = analyzeQuoteSymbol((SymbolElement) quotedObject);
		} else if (quotedObject instanceof ConsElement) {
			analyzedElement = analyzeQuoteList((ConsElement) quotedObject);
		}

		final Element element;

		if (analyzedElement == null) {
			element = analyzer.analyzeForm(quotedObject);
		} else {
			// If was ListStruct or SymbolStruct, wrap resulting form in Load-Time-Value.
			final ConsElement loadTimeValueForm = new ConsElement(SpecialOperatorElement.LOAD_TIME_VALUE, analyzedElement);
			element = loadTimeValueAnalyzer.analyze(analyzer, loadTimeValueForm, analysisBuilder);
		}

		return new QuoteElement(element);
	}

	private static ConsElement analyzeQuoteList(final ConsElement element) {
		final SymbolElement listFnSym;
		if (element.isDotted()) {
			listFnSym = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "LIST*");
		} else {
			listFnSym = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "LIST");
		}

		final List<? extends SimpleElement> formJavaList = element.getElements();

		final EnhancedLinkedList<SimpleElement> transformedListForms = new EnhancedLinkedList<>();
		transformedListForms.add(listFnSym);
		transformedListForms.addAll(formJavaList);

		return new ConsElement(transformedListForms);
	}

	private static ConsElement analyzeQuoteSymbol(final SymbolElement element) {
		final String symbolFunctionString;
		final String symbolNameString;

		// TODO: need to review this a bit...
		final String symbolPackage = element.getPackageName();
		if (symbolPackage != null) {
			symbolFunctionString = "FIND-SYMBOL";
			symbolNameString = symbolPackage;
		} else {
			symbolFunctionString = "MAKE-SYMBOL";
			symbolNameString = element.getSymbolName();
		}

		final SymbolElement symbolFnSym = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), symbolFunctionString);
		final StringElement symbolNameStringStruct = new StringElement(symbolNameString);

		final EnhancedLinkedList<SimpleElement> symbolQuoteList = new EnhancedLinkedList<>();
		symbolQuoteList.add(symbolFnSym);
		symbolQuoteList.add(symbolNameStringStruct);

		return new ConsElement(symbolQuoteList);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
