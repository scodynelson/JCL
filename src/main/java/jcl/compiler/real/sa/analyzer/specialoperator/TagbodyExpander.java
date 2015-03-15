package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStructGenerator;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TagbodyExpander extends MacroFunctionExpander<TagbodyStruct> {

	private static final long serialVersionUID = -1543233114989622747L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Resource
	private Map<Class<? extends LispStruct>, GoStructGenerator<LispStruct>> goStructGeneratorStrategies;

	/**
	 * Initializes the tagbody macro function and adds it to the special operator 'tagbody'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.TAGBODY.setMacroFunctionExpander(this);
	}

	@Override
	public TagbodyStruct expand(final ListStruct form, final Environment environment) {

		ListStruct body = form.getRest();
		List<LispStruct> bodyAsJavaList = body.getAsJavaList();

		final TagbodyTagSetCollector tagSetCollector = new TagbodyTagSetCollector(goStructGeneratorStrategies);
		final Set<GoStruct<?>> tagSet = bodyAsJavaList.stream()
		                                              .collect(tagSetCollector);

		environment.getTagbodyStack().push(tagSet);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		// NOTE: We don't care about adding this to the TagbodyStack since it is generated just for us here and will
		//       will never be used as a real transfer of control point.
		if (!isTagbodyTag(body.getFirst())) {
			final SymbolStruct<?> defaultFormsTag = new SymbolStruct<>("Tag" + UUID.randomUUID());
			body = new ConsStruct(defaultFormsTag, body);
			bodyAsJavaList = body.getAsJavaList();
		}

		try {
			final TagbodyCollector collector = new TagbodyCollector(formAnalyzer, environment, goStructGeneratorStrategies);
			final Map<GoStruct<?>, List<LispStruct>> tagbodyForms = bodyAsJavaList.stream()
			                                                                      .collect(collector);
			return new TagbodyStruct(tagbodyForms);
		} finally {
			environment.getTagbodyStack().pop();
		}
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
