package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.struct.specialoperator.LocallyStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LocallyExpander extends MacroFunctionExpander<LocallyStruct> {

	private static final long serialVersionUID = 8925649944409732052L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private DeclareExpander declareExpander;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	/**
	 * Initializes the locally macro function and adds it to the special operator 'locally'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.LOCALLY.setMacroFunctionExpander(this);
	}

	@Override
	public LocallyStruct expand(final ListStruct form, final Environment environment) {

		final Environment locallyEnvironment = new Environment(environment);

		final ListStruct formRest = form.getRest();
		final List<LispStruct> forms = formRest.getAsJavaList();

		final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.buildProperList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = declareExpander.expand(fullDeclaration, locallyEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.forEach(specialDeclaration -> Environments.addDynamicVariableBinding(specialDeclaration, locallyEnvironment));

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> formAnalyzer.analyze(e, locallyEnvironment))
				           .collect(Collectors.toList());

		return new LocallyStruct(new PrognStruct(analyzedBodyForms), locallyEnvironment);
	}
}
