/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.NewMacroExpand;
import jcl.compiler.real.sa.analyzer.expander.real.NewMacroExpandReturn;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FormAnalyzerImpl implements FormAnalyzer {

	private static final long serialVersionUID = 7315325926130864447L;

	@Autowired
	private NewMacroExpand newMacroExpand;

	@Resource
	private Map<Class<? extends LispStruct>, Analyzer<? extends LispStruct, LispStruct>> analyzerStrategies;

	@Override
	public LispStruct analyze(final LispStruct input, final Environment environment) {

		final NewMacroExpandReturn macroExpandReturn = newMacroExpand.macroExpand(input, environment);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		final Analyzer<? extends LispStruct, LispStruct> functionCallAnalyzer = analyzerStrategies.get(expandedForm.getClass());
		if (functionCallAnalyzer == null) {
			return expandedForm; // TODO: we need to rework the logic a bit, so for now we just return...
//			throw new ProgramErrorException("Semantic Analyzer: Unsupported object type cannot be analyzed: " + expandedForm);
		}

		return functionCallAnalyzer.analyze(expandedForm, environment);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
