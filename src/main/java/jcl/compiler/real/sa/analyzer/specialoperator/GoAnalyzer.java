package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.GoElement;
import jcl.compiler.real.element.specialoperator.GoIntegerElement;
import jcl.compiler.real.element.specialoperator.GoSymbolElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;
import jcl.util.InstanceOf;
import org.springframework.stereotype.Component;

import java.util.ListIterator;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;

@Component
public class GoAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -6523523596100793498L;

	@Override
	public GoElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();

		final Optional<GoElement> tagToFind
				= InstanceOf.when(second)
				            .isInstanceOf(SymbolStruct.class).thenReturn(GoAnalyzer::getGoSymbolElementTag)
				            .isInstanceOf(IntegerStruct.class).thenReturn(GoAnalyzer::getGoIntegerElementTag)
				            .get();

		if (tagToFind.isPresent()) {
			final GoElement realTagToFind = tagToFind.get();
			return getGoTag(analysisBuilder, realTagToFind);
		} else {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}
	}

	private GoElement getGoTag(final AnalysisBuilder analysisBuilder, final GoElement tagToFind) {

		final Stack<Set<GoElement>> tagbodyStack = analysisBuilder.getTagbodyStack();
		final ListIterator<Set<GoElement>> tagbodyListIterator = tagbodyStack.listIterator(tagbodyStack.size());

		GoElement tag = null;

		while (tagbodyListIterator.hasPrevious()) {
			final Set<GoElement> previousStack = tagbodyListIterator.previous();
			if (previousStack.contains(tagToFind)) {
				tag = tagToFind;
				break;
			}
		}

		if (tag == null) {
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + tagToFind + " is visible.");
		}

		return tag;
	}

	private static GoElement getGoSymbolElementTag(final SymbolStruct<?> symbolStruct) {
		final SymbolElement<?> symbolElement = new SymbolElement<>(symbolStruct);
		return new GoSymbolElement(symbolElement);
	}

	private static GoElement getGoIntegerElementTag(final IntegerStruct integerStruct) {
		final IntegerElement integerElement = new IntegerElement(integerStruct);
		return new GoIntegerElement(integerElement);
	}
}
