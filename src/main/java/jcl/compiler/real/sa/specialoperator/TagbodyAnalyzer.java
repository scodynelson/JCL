package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class TagbodyAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final TagbodyAnalyzer INSTANCE = new TagbodyAnalyzer();

	public static Stack<Map<LispStruct, SymbolStruct<?>>> tagbodyStack;
	public static int iTagbodyCounter;

	@Override
	public LispStruct analyze(final ListStruct input) {
		// Read all the tags within the TAGBODY.
		final ListStruct body = input.getRest();
		final List<LispStruct> bodyJavaList = body.getAsJavaList();
		final Map<LispStruct, SymbolStruct<?>> currentTagMap = readTagbodyLabels(bodyJavaList);

		tagbodyStack.push(currentTagMap);

		final List<LispStruct> newBodyJavaList = new ArrayList<>();

		for (final LispStruct currentBodyElement : bodyJavaList) {
			if ((currentBodyElement instanceof SymbolStruct) || (currentBodyElement instanceof IntegerStruct)) {
				final SymbolStruct<?> realTagSymbol = currentTagMap.get(currentBodyElement);
				newBodyJavaList.add(realTagSymbol);
			} else {
				final LispStruct analyzedElement = SemanticAnalyzer.saMainLoop(currentBodyElement);
				newBodyJavaList.add(analyzedElement);
			}
		}

		// Pop the tag stack for this TAGBODY from the global stack.
		tagbodyStack.pop();

		final List<LispStruct> tagbodyResult = new ArrayList<>();
		tagbodyResult.add(SpecialOperator.TAGBODY);
		tagbodyResult.addAll(newBodyJavaList);

		return ListStruct.buildProperList(tagbodyResult);
	}

	// Reads all the tags in the TAGBODY form and inserts them into a stack
	// which is returned. Its necessary to do this first since a GO can be
	// executed for a tag declared later in the form.
	private static Map<LispStruct, SymbolStruct<?>> readTagbodyLabels(final List<LispStruct> list) {
		final Map<LispStruct, SymbolStruct<?>> currentTagMap = new HashMap<>();

		for (final LispStruct current : list) {
			if ((current instanceof SymbolStruct) || (current instanceof NumberStruct)) {
				// Insert the tag and its new SymbolStruct into the stack.
				final SymbolStruct<?> newSym = new SymbolStruct("Tagbody" + iTagbodyCounter);
				iTagbodyCounter++;
				currentTagMap.put(current, newSym);
			}
		}
		return currentTagMap;
	}
}
