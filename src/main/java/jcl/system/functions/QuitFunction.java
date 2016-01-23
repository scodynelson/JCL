/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import jcl.LispStruct;
import jcl.functions.AbstractExtensionsFunctionStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class QuitFunction extends AbstractExtensionsFunctionStruct {

	@Autowired
	private ApplicationContext context;

	public QuitFunction() {
		super("Quits the JCL Application.");
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		System.exit(SpringApplication.exit(context));
		return null;
	}

	@Override
	protected String functionName() {
		return "QUIT";
	}
}
