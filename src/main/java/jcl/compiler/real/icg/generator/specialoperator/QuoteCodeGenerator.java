package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.SpecialVariableCodeGenerator;
import jcl.compiler.real.icg.generator.simple.ComplexCodeGenerator;
import jcl.compiler.real.icg.generator.simple.FloatCodeGenerator;
import jcl.compiler.real.icg.generator.simple.IntegerCodeGenerator;
import jcl.compiler.real.icg.generator.simple.RatioCodeGenerator;
import jcl.lists.ListStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuoteCodeGenerator implements CodeGenerator<ListStruct> {

	// this method can ONLY handle simple constants such as numbers, strings,
	// and literal symbols

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Autowired
	private IntegerCodeGenerator integerCodeGenerator;

	@Autowired
	private FloatCodeGenerator floatCodeGenerator;

	@Autowired
	private RatioCodeGenerator ratioCodeGenerator;

	@Autowired
	private ComplexCodeGenerator complexCodeGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {
		final Object quotedObj = input.getRest().getFirst();
		if (quotedObj instanceof SymbolStruct) {
			final SymbolStruct<?> sym = (SymbolStruct<?>) quotedObj;
			//TODO work out a way to handle uninterned symbols that have been encountered already
			// need symbol package lookup here!
			if (sym.getSymbolPackage() == null) {
				classBuilder.getEmitter().emitLdc(sym.getName());
				classBuilder.getEmitter().emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
			} else {
				specialVariableCodeGenerator.generate(sym, classBuilder);
			}
		} else if (quotedObj instanceof IntegerStruct) {
			integerCodeGenerator.generate((IntegerStruct) quotedObj, classBuilder);
		} else if (quotedObj instanceof FloatStruct) {
			floatCodeGenerator.generate((FloatStruct) quotedObj, classBuilder);
		} else if (quotedObj instanceof RatioStruct) {
			ratioCodeGenerator.generate((RatioStruct) quotedObj, classBuilder);
//		} else if (quotedObj instanceof ComplexStruct) {
//			complexCodeGenerator.generate((ComplexStruct) quotedObj, codeGenerator, classBuilder);
		} else {
			throw new RuntimeException("Unable to quote: " + quotedObj);
		}
	}
}
