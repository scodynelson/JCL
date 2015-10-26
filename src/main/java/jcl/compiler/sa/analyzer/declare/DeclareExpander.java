package jcl.compiler.sa.analyzer.declare;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DeclareExpander extends MacroFunctionExpander<DeclareStruct> {

	private static final long serialVersionUID = -27949883247210201L;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.DECLARE;
	}

	@Override
	public DeclareStruct expand(final ListStruct form, final Environment environment) {

		final List<LispStruct> declSpecs = form.getRest().getAsJavaList();

		final DeclareStruct declareElement = new DeclareStruct();

		for (final LispStruct declSpec : declSpecs) {
			final ListStruct declSpecList = validator.validateObjectType(declSpec, "DECLARE", "DECLARATION SPECIFIER", ListStruct.class);
			final LispStruct declIdentifier = declSpecList.getFirst();
			final ListStruct declSpecBody = declSpecList.getRest();

			final List<LispStruct> declSpecBodyAsJavaList = declSpecBody.getAsJavaList();

			// now come the various cases
			if (declIdentifier.equals(DeclarationStruct.DECLARATION)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.DYNAMIC_EXTENT)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.FTYPE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.IGNORABLE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.IGNORE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.INLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.NOTINLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.OPTIMIZE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.JAVA_CLASS_NAME)) {
				final JavaClassNameDeclarationStruct jclds = saJavaClassNameDeclaration(declSpecBodyAsJavaList);
				declareElement.setJavaClassNameDeclaration(jclds);
			} else if (declIdentifier.equals(DeclarationStruct.SPECIAL)) {
				final List<SpecialDeclarationStruct> sds = saSpecialDeclaration(declSpecBodyAsJavaList);
				declareElement.getSpecialDeclarations().addAll(sds);
			} else if (declIdentifier.equals(DeclarationStruct.TYPE)) {
				//TODO: we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private JavaClassNameDeclarationStruct saJavaClassNameDeclaration(final List<LispStruct> declSpecBody) {

		final int declSpecBodySize = declSpecBody.size();
		if (declSpecBodySize != 1) {
			throw new ProgramErrorException("DECLARE: Incorrect number of arguments for JAVA-CLASS-NAME declaration: " + declSpecBodySize + ". Expected 1 argument.");
		}

		final LispStruct javaClassName = declSpecBody.get(0);
		final StringStruct javaClassNameString = validator.validateObjectType(javaClassName, "DECLARE", "JAVA-CLASS-NAME", StringStruct.class);
		return new JavaClassNameDeclarationStruct(javaClassNameString.getAsJavaString());
	}

	private List<SpecialDeclarationStruct> saSpecialDeclaration(final List<LispStruct> declSpecBody) {

		final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				final String printedObject = printer.print(declSpecBodyElement);
				throw new ProgramErrorException("DECLARE: a non-symbol cannot be made SPECIAL: " + printedObject);
			}

			final SymbolStruct<?> sym = (SymbolStruct<?>) declSpecBodyElement;

			final SpecialDeclarationStruct specialDeclarationElement = new SpecialDeclarationStruct(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}
}
