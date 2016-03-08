package jcl.sequences.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.sequences.SequenceStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ReverseFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public ReverseFunction() {
		super("Returns a new sequence of the same kind as sequence, containing the same elements, but in reverse order.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SEQUENCE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SequenceStruct sequence
				= validator.validateType(lispStructs[0], functionName(), "Sequence", ListType.INSTANCE, SequenceStruct.class);
		return sequence.reverse();
	}

	@Override
	protected String functionName() {
		return "REVERSE";
	}
}
