package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PairlisFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public PairlisFunction() {
		super("Returns an association list that associates elements of keys to corresponding elements of data.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "KEYS").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "DATUMS").build()
		);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "ALIST")
		                        .suppliedPBinding()
		                        .initForm(NullStruct.INSTANCE)
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct keys = validator.validateType(lispStructs[0], functionName(), "Keys", ListType.INSTANCE, ListStruct.class);
		final ListStruct datums = validator.validateType(lispStructs[1], functionName(), "Datums", ListType.INSTANCE, ListStruct.class);

		final int keysLength = keys.size();
		final int datumsLength = datums.size();
		if (keysLength != datumsLength) {
			throw new SimpleErrorException("The lists of keys and datums are not the same length.");
		}

		final LispStruct[] keysArray = keys.toArray();
		final LispStruct[] datumsArray = datums.toArray();

		ListStruct alist = NullStruct.INSTANCE;
		if (lispStructs.length > 2) {
			alist = validator.validateType(lispStructs[2], functionName(), "Association-List", ListType.INSTANCE, ListStruct.class);
		}

		for (int i = 0; i < keysLength; i++) {
			final LispStruct key = keysArray[i];
			final LispStruct datum = datumsArray[i];
			final ConsStruct pair = new ConsStruct(key, datum);
			alist = new ConsStruct(pair, alist);
		}

		return alist;
	}

	@Override
	protected String functionName() {
		return "PAIRLIS";
	}
}
