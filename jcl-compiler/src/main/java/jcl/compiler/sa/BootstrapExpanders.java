package jcl.compiler.sa;

import jcl.compiler.sa.analyzer.LambdaExpander;
import jcl.compiler.sa.analyzer.MacroLambdaExpander;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.sa.analyzer.defstruct.DefstructExpander;
import jcl.compiler.sa.analyzer.specialoperator.BlockExpander;
import jcl.compiler.sa.analyzer.specialoperator.CatchExpander;
import jcl.compiler.sa.analyzer.specialoperator.EvalWhenExpander;
import jcl.compiler.sa.analyzer.specialoperator.FletExpander;
import jcl.compiler.sa.analyzer.specialoperator.FunctionExpander;
import jcl.compiler.sa.analyzer.specialoperator.GoExpander;
import jcl.compiler.sa.analyzer.specialoperator.IfExpander;
import jcl.compiler.sa.analyzer.specialoperator.LabelsExpander;
import jcl.compiler.sa.analyzer.specialoperator.LetExpander;
import jcl.compiler.sa.analyzer.specialoperator.LetStarExpander;
import jcl.compiler.sa.analyzer.specialoperator.LoadTimeValueExpander;
import jcl.compiler.sa.analyzer.specialoperator.LocallyExpander;
import jcl.compiler.sa.analyzer.specialoperator.MacroletExpander;
import jcl.compiler.sa.analyzer.specialoperator.MultipleValueCallExpander;
import jcl.compiler.sa.analyzer.specialoperator.MultipleValueProg1Expander;
import jcl.compiler.sa.analyzer.specialoperator.PrognExpander;
import jcl.compiler.sa.analyzer.specialoperator.ProgvExpander;
import jcl.compiler.sa.analyzer.specialoperator.QuoteExpander;
import jcl.compiler.sa.analyzer.specialoperator.ReturnFromExpander;
import jcl.compiler.sa.analyzer.specialoperator.SetqExpander;
import jcl.compiler.sa.analyzer.specialoperator.SymbolMacroletExpander;
import jcl.compiler.sa.analyzer.specialoperator.TagbodyExpander;
import jcl.compiler.sa.analyzer.specialoperator.TheExpander;
import jcl.compiler.sa.analyzer.specialoperator.ThrowExpander;
import jcl.compiler.sa.analyzer.specialoperator.UnwindProtectExpander;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class BootstrapExpanders {

	public static void bootstrap() throws Exception {
		LambdaExpander.INSTANCE.afterPropertiesSet();
		MacroLambdaExpander.INSTANCE.afterPropertiesSet();

		DeclareExpander.INSTANCE.afterPropertiesSet();

		DefstructExpander.INSTANCE.afterPropertiesSet();

		BlockExpander.INSTANCE.afterPropertiesSet();
		CatchExpander.INSTANCE.afterPropertiesSet();
		EvalWhenExpander.INSTANCE.afterPropertiesSet();
		FletExpander.INSTANCE.afterPropertiesSet();
		FunctionExpander.INSTANCE.afterPropertiesSet();
		GoExpander.INSTANCE.afterPropertiesSet();
		IfExpander.INSTANCE.afterPropertiesSet();
		LabelsExpander.INSTANCE.afterPropertiesSet();
		LetExpander.INSTANCE.afterPropertiesSet();
		LetStarExpander.INSTANCE.afterPropertiesSet();
		LoadTimeValueExpander.INSTANCE.afterPropertiesSet();
		LocallyExpander.INSTANCE.afterPropertiesSet();
		MacroletExpander.INSTANCE.afterPropertiesSet();
		MultipleValueCallExpander.INSTANCE.afterPropertiesSet();
		MultipleValueProg1Expander.INSTANCE.afterPropertiesSet();
		PrognExpander.INSTANCE.afterPropertiesSet();
		ProgvExpander.INSTANCE.afterPropertiesSet();
		QuoteExpander.INSTANCE.afterPropertiesSet();
		ReturnFromExpander.INSTANCE.afterPropertiesSet();
		SetqExpander.INSTANCE.afterPropertiesSet();
		SymbolMacroletExpander.INSTANCE.afterPropertiesSet();
		TagbodyExpander.INSTANCE.afterPropertiesSet();
		TheExpander.INSTANCE.afterPropertiesSet();
		ThrowExpander.INSTANCE.afterPropertiesSet();
		UnwindProtectExpander.INSTANCE.afterPropertiesSet();
	}
}
