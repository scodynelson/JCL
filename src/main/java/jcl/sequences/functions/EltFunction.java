package jcl.sequences.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.sequences.SequenceStruct;
import jcl.types.IntegerType;
import jcl.types.SequenceType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class EltFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator typeValidator;

	public EltFunction() {
		super("Accesses the element of sequence specified by index.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SEQUENCE").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INDEX").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SequenceStruct sequence
				= typeValidator.validateType(lispStructs[0], functionName(), "Sequence", SequenceType.INSTANCE, SequenceStruct.class);
		final IntegerStruct index
				= typeValidator.validateType(lispStructs[1], functionName(), "Index", IntegerType.INSTANCE, IntegerStruct.class);

		final long indexValue = index.getBigInteger().longValue();
		return sequence.elt(indexValue);
	}

	@Override
	protected String functionName() {
		return "ELT";
	}
}
