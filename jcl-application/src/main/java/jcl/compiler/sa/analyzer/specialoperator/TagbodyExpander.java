package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.TagbodyStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.number.IntegerStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TagbodyExpander extends MacroFunctionExpander<TagbodyStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.TAGBODY;
	}

	@Override
	public TagbodyStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // TAGBODY SYMBOL

		// Using LinkedList here in case we have to push a 'default' tag on the front
		final LinkedList<LispStruct> forms = new LinkedList<>();
		iterator.forEachRemaining(forms::add);

		final TagbodyTagSetCollector tagSetCollector = new TagbodyTagSetCollector();
		final List<GoStruct<?>> tagList = forms.stream()
		                                       .collect(tagSetCollector);
		environment.getTagbodyStack().push(tagList);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		// NOTE: We don't care about adding this to the TagbodyStack since it is generated just for us here and will
		//       will never be used as a real transfer of control point.
		if (!forms.isEmpty() && !isTagbodyTag(forms.get(0))) {
			final SymbolStruct defaultFormsTag = new SymbolStruct("Tag-" + UUID.randomUUID());
			forms.push(defaultFormsTag);
		}

		try {
			final TagbodyFormCollector tagbodyFormCollector = new TagbodyFormCollector(formAnalyzer, environment);
			final Map<GoStruct<?>, PrognStruct> analyzedTagbodyForms = forms.stream()
			                                                                .collect(tagbodyFormCollector);
			return new TagbodyStruct(analyzedTagbodyForms);
		} finally {
			environment.getTagbodyStack().pop();
		}
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}
}
