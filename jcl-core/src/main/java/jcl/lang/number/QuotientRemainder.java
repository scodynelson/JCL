/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import jcl.lang.RealStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class QuotientRemainder {

	private final RealStruct quotient;

	private final RealStruct remainder;
}
