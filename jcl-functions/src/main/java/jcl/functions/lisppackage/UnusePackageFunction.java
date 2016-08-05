/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unuse-package}.
 */
@Component
public final class UnusePackageFunction extends AbstractPackageListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public UnusePackageFunction() {
		super("Causes package to cease inheriting all the external symbols of packages-to-unuse.",
		      "UNUSE-PACKAGE"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStructImpl#unUsePackage(PackageStructImpl...)} as a method reference function.
	 *
	 * @return returns {@link PackageStructImpl#unUsePackage(PackageStructImpl...)} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStructImpl, PackageStructImpl[]> packageListFunction() {
		return PackageStructImpl::unUsePackage;
	}

	/**
	 * {@inheritDoc}
	 * Performs default (aka. no) validation on the provided {@link PackageStructImpl}s.
	 *
	 * @param packageStructs
	 * 		the {@link PackageStructImpl}s to validate
	 */
	@Override
	protected void validatePackages(final PackageStructImpl... packageStructs) {
	}
}
