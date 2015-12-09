/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-nicknames}.
 */
@Component
public final class PackageNicknamesFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5582005245052509872L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageNicknamesFunction() {
		super("Returns the list of nickname strings for package, not including the name of package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = findPackage(lispStruct);

		final List<String> nicknames = aPackage.getNicknames();
		final List<LispStruct> nicknamesStructs =
				nicknames.stream()
				         .map(StringStruct::new)
				         .collect(Collectors.toList());
		return ListStruct.buildProperList(nicknamesStructs);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-nicknames} as a string.
	 *
	 * @return the function name {@code package-nicknames} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-NICKNAMES";
	}
}
