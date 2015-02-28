package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.go.GoElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;

@Component
public class GoAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -6523523596100793498L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.GO.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public GoElement<?> analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();

		if (!isTagbodyTag(second)) {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		return getGoTag(analysisBuilder, second);
	}

	private static boolean isTagbodyTag(final SimpleElement element) {
		return (element instanceof SymbolElement) || (element instanceof IntegerElement);
	}

	private static GoElement<?> getGoTag(final AnalysisBuilder analysisBuilder, final SimpleElement tagToFind) {

		final Stack<Set<GoElement<?>>> tagbodyStack = analysisBuilder.getTagbodyStack();
		final ListIterator<Set<GoElement<?>>> tagbodyListIterator = tagbodyStack.listIterator(tagbodyStack.size());

		GoElement<?> tag = null;

		out:
		while (tagbodyListIterator.hasPrevious()) {
			final Set<GoElement<?>> previousStack = tagbodyListIterator.previous();
			for (final GoElement<?> goElement : previousStack) {
				final SimpleElement goElementTag = goElement.getTag();
				if (tagToFind.equals(goElementTag)) {
					tag = goElement;
					break out;
				}
			}
		}

		if (tag == null) {
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + tagToFind + " is visible.");
		}

		return tag;
	}
}
