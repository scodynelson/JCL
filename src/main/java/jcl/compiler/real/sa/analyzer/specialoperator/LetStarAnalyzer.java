package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.specialoperator.LetElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

@Component
public class LetStarAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private LetAnalyzer letAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LET_STAR.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public LetElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LET*: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ListElement)) {
			throw new ProgramErrorException("LET*: Parameter list must be of type List. Got: " + second);
		}

		final ListElement parameters = (ListElement) second;
		final List<? extends SimpleElement> parametersAsJavaList = parameters.getElements();

		final ListIterator<? extends SimpleElement> iterator = parametersAsJavaList.listIterator(parametersAsJavaList.size());

		EnhancedLinkedList<SimpleElement> body = inputRest.getAllButFirst();

		while (iterator.hasPrevious()) {
			final SimpleElement previousParams = iterator.previous();

			final List<SimpleElement> innerLet = new ArrayList<>();
			innerLet.add(SpecialOperatorElement.LET);
			innerLet.add(previousParams);
			innerLet.addAll(body);

			body = new EnhancedLinkedList<>(innerLet);
		}

		return letAnalyzer.analyze(new ConsElement(body), analysisBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
