package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.TagbodyStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.compiler.struct.specialoperator.go.GoStructFactory;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TagbodyExpander extends MacroFunctionExpander<TagbodyStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Resource
	private Map<Class<? extends LispStruct>, GoStructFactory<LispStruct>> goStructFactoryStrategies;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.TAGBODY;
	}

	@Override
	public TagbodyStruct expand(final ListStruct form, final Environment environment) {

		ListStruct formRest = form.getRest();
		List<LispStruct> forms = formRest.getAsJavaList();

		final TagbodyTagSetCollector tagSetCollector = new TagbodyTagSetCollector(goStructFactoryStrategies);
		final List<GoStruct<?>> tagList = forms.stream()
		                                       .collect(tagSetCollector);

		environment.getTagbodyStack().push(tagList);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		// NOTE: We don't care about adding this to the TagbodyStack since it is generated just for us here and will
		//       will never be used as a real transfer of control point.
		if (!isTagbodyTag(formRest.getCar())) {
			final SymbolStruct defaultFormsTag = new SymbolStruct("Tag-" + UUID.randomUUID());
			formRest = new ConsStruct(defaultFormsTag, formRest);
			forms = formRest.getAsJavaList();
		}

		try {
			final TagbodyFormCollector tagbodyFormCollector = new TagbodyFormCollector(formAnalyzer, environment, goStructFactoryStrategies);
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
