package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarExpander extends MacroFunctionExpander<LetStruct> {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private LetExpander letExpander;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LET_STAR.setMacroFunctionExpander(this);
	}

	@Override
	public LetStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("LET*: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LET*: Parameter list must be of type List. Got: " + second);
		}

		final ListStruct parameters = (ListStruct) second;
		final List<? extends LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListIterator<? extends LispStruct> iterator = parametersAsJavaList.listIterator(parametersAsJavaList.size());

		List<LispStruct> body = inputRest.getRest().getAsJavaList();

		while (iterator.hasPrevious()) {
			final LispStruct previousParams = iterator.previous();

			final List<LispStruct> innerLet = new ArrayList<>();
			innerLet.add(SpecialOperator.LET);
			innerLet.add(previousParams);
			innerLet.addAll(body);

			body = innerLet;
		}

		return letExpander.expand(ListStruct.buildProperList(body), analysisBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
