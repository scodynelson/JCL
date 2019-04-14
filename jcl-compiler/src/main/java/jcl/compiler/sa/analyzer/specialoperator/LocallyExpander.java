package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.sa.analyzer.body.BodyWithDeclaresAnalyzer;
import jcl.compiler.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.struct.specialoperator.LocallyStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class LocallyExpander extends MacroFunctionExpander<LocallyStruct> {

	public static final LocallyExpander INSTANCE = new LocallyExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.LOCALLY;
	}

	@Override
	public LocallyStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // LOCALLY SYMBOL

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final Environment locallyEnvironment = new Environment(environment);

		final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.analyze(forms);

		final ListStruct fullDeclaration = ListStruct.toLispList(bodyProcessingResult.getDeclares());
		final DeclareStruct declare = DeclareExpander.INSTANCE.expand(fullDeclaration, locallyEnvironment);

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		specialDeclarations.stream()
		                   .map(SpecialDeclarationStruct::getVar)
		                   .map(Binding::new)
		                   .forEach(locallyEnvironment::addDynamicBinding);

		final List<LispStruct> bodyForms = bodyProcessingResult.getBodyForms();

		final List<LispStruct> analyzedBodyForms
				= bodyForms.stream()
				           .map(e -> FormAnalyzer.analyze(e, locallyEnvironment))
				           .collect(Collectors.toList());

		return new LocallyStruct(analyzedBodyForms, locallyEnvironment);
	}
}
