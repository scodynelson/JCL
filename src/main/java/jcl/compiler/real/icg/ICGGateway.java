/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;

@MessagingGateway
public interface ICGGateway {

	@Gateway(requestChannel = "generationChannel")
	GeneratorState generate(ICGPayload input);
}
