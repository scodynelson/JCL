package jcl.sequences.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.sequences.SequenceStruct;
import jcl.types.IntegerType;
import jcl.types.SequenceType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetEltFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetEltFunction() {
		super("Sets the element of sequence specified by index to the new-value provided.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "LIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "INDEX").build(),
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "VALUE").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SequenceStruct sequence
				= validator.validateType(lispStructs[0], functionName(), "Sequence", SequenceType.INSTANCE, SequenceStruct.class);
		final IntegerStruct index
				= validator.validateType(lispStructs[1], functionName(), "Index", IntegerType.INSTANCE, IntegerStruct.class);
		final LispStruct value = lispStructs[2];

		final long indexValue = index.getBigInteger().longValue();
		sequence.setElt(indexValue, value);
		return value;
	}

	@Override
	protected String functionName() {
		return "SET-ELT";
	}
}
