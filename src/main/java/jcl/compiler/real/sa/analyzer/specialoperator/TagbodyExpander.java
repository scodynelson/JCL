package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStructGenerator;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
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
		SpecialOperatorStruct.TAGBODY.setMacroFunctionExpander(this);
	}

	@Override
	public TagbodyStruct expand(final ListStruct form, final Environment environment) {

		ListStruct formRest = form.getRest();
		List<LispStruct> forms = formRest.getAsJavaList();

		final TagbodyTagSetCollector tagSetCollector = new TagbodyTagSetCollector(goStructGeneratorStrategies);
		final List<GoStruct<?>> tagList = forms.stream()
		                                       .collect(tagSetCollector);

		environment.getTagbodyStack().push(tagList);

		// If the first element is not a 'tag', we have a default form set. Therefore, we are going to generate a
		// temporary 'tag' for this form set.
		// NOTE: We don't care about adding this to the TagbodyStack since it is generated just for us here and will
		//       will never be used as a real transfer of control point.
		if (!isTagbodyTag(formRest.getFirst())) {
			final SymbolStruct<?> defaultFormsTag = new SymbolStruct<>("Tag-" + UUID.randomUUID());
			formRest = new ConsStruct(defaultFormsTag, formRest);
			forms = formRest.getAsJavaList();
		}

		try {
			final TagbodyFormCollector tagbodyFormCollector = new TagbodyFormCollector(formAnalyzer, environment, goStructGeneratorStrategies);
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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(goStructGeneratorStrategies)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final TagbodyExpander rhs = (TagbodyExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(goStructGeneratorStrategies, rhs.goStructGeneratorStrategies)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(goStructGeneratorStrategies)
		                                                                .toString();
	}
}
