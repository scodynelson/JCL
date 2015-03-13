package jcl.compiler.real.sa.analyzer.declare;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DeclareExpander extends MacroFunctionExpander<DeclareStruct> {

	private static final long serialVersionUID = -27949883247210201L;

	@Autowired
	private SymbolAnalyzer symbolAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.DECLARE.setMacroFunctionExpander(this);
	}

	@Override
	public DeclareStruct expand(final ListStruct form, final Environment environment) {

		final List<LispStruct> declSpecs = form.getRest().getAsJavaList();

		final DeclareStruct declareElement = new DeclareStruct();

		for (final LispStruct declSpec : declSpecs) {

			if (!(declSpec instanceof ListStruct)) {
				throw new ProgramErrorException("DECLARE: Declaration specifier must be of type ListStruct. Got: " + declSpec);
			}

			final ListStruct declSpecList = (ListStruct) declSpec;
			final LispStruct declIdentifier = declSpecList.getFirst();
			final ListStruct declSpecBody = declSpecList.getRest();

			// now come the various cases
			if (declIdentifier.equals(Declaration.DECLARATION)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.DYNAMIC_EXTENT)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.FTYPE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORABLE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.INLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.NOTINLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.OPTIMIZE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.SPECIAL)) {
				final List<SpecialDeclarationStruct> sdes = saSpecialDeclaration(environment, declSpecBody.getAsJavaList());
				declareElement.getSpecialDeclarationElements().addAll(sdes);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
				//we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private List<SpecialDeclarationStruct> saSpecialDeclaration(final Environment environment, final List<LispStruct> declSpecBody) {

		final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: " + declSpecBodyElement);
			}

			final SymbolStruct<?> sym = (SymbolStruct<?>) declSpecBodyElement;

			symbolAnalyzer.analyzeDynamic(sym, environment);

			final SpecialDeclarationStruct specialDeclarationElement = new SpecialDeclarationStruct(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}
}
