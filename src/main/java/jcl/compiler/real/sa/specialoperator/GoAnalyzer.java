package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.GoElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ListIterator;
import java.util.Map;
import java.util.Stack;

@Component
public class GoAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof NumberStruct)) {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		SymbolStruct<?> tag = null;

		final Stack<Map<LispStruct, SymbolStruct<?>>> tagbodyStack = analysisBuilder.getTagbodyStack();
		final ListIterator<Map<LispStruct, SymbolStruct<?>>> li1 = tagbodyStack.listIterator(tagbodyStack.size());

		while (li1.hasPrevious()) {
			final Map<LispStruct, SymbolStruct<?>> previousStack = li1.previous();
			if (previousStack.containsKey(second)) {
				tag = previousStack.get(second);
				break;
			}
		}

		if (tag == null) {
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + second + " is visible.");
		}

		return new GoElement(tag);
	}
}
