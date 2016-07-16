package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.LocallyStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LocallyExpander extends MacroFunctionExpander<LocallyStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.LOCALLY;
	}

	@Override
	public LocallyStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // LOCALLY SYMBOL

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final Environment locallyEnvironment = new Environment(environment);

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, locallyEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(locallyEnvironment::addDynamicBinding);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, locallyEnvironment))
				           .collect(Collectors.toList());

		return new LocallyStruct(analyzedBodyForms, locallyEnvironment);
	}
}
