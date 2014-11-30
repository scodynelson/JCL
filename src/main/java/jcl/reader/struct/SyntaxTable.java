/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.struct;

import jcl.characters.CharacterConstants;
import jcl.reader.SyntaxType;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Represents a lookup table for {@link SyntaxType}s matching {@link Integer} code points.
 */
@SuppressWarnings("all")
class SyntaxTable {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SyntaxTable.class);

	/**
	 * The internal mapping of character code points to {@link SyntaxType}s.
	 */
	private final Map<Integer, SyntaxType> syntaxTypeMap;

	/**
	 * Package constructor.
	 */
	SyntaxTable() {
		syntaxTypeMap = new ConcurrentHashMap<>(128);

		syntaxTypeMap.put(0, SyntaxType.INVALID);                     // NUL
		syntaxTypeMap.put(1, SyntaxType.INVALID);                     // SOH
		syntaxTypeMap.put(2, SyntaxType.INVALID);                     // STX
		syntaxTypeMap.put(3, SyntaxType.INVALID);                     // ETX
		syntaxTypeMap.put(4, SyntaxType.INVALID);                     // EOT
		syntaxTypeMap.put(5, SyntaxType.INVALID);                     // ENQ
		syntaxTypeMap.put(6, SyntaxType.INVALID);                     // ACK
		syntaxTypeMap.put(7, SyntaxType.INVALID);                     // BEL
		syntaxTypeMap.put(8, SyntaxType.CONSTITUENT);                 // BACKSPACE
		syntaxTypeMap.put(9, SyntaxType.WHITESPACE);                  // HORIZONTAL TAB
		syntaxTypeMap.put(10, SyntaxType.WHITESPACE);                 // NEWLINE
		syntaxTypeMap.put(11, SyntaxType.WHITESPACE);                 // VERTICAL TAB
		syntaxTypeMap.put(12, SyntaxType.WHITESPACE);                 // FORM FEED
		syntaxTypeMap.put(13, SyntaxType.WHITESPACE);                 // CARRIAGE RETURN
		syntaxTypeMap.put(14, SyntaxType.INVALID);                    // SO
		syntaxTypeMap.put(15, SyntaxType.INVALID);                    // SI
		syntaxTypeMap.put(16, SyntaxType.INVALID);                    // DLE
		syntaxTypeMap.put(17, SyntaxType.INVALID);                    // DC1
		syntaxTypeMap.put(18, SyntaxType.INVALID);                    // DC2
		syntaxTypeMap.put(19, SyntaxType.INVALID);                    // DC3
		syntaxTypeMap.put(20, SyntaxType.INVALID);                    // DC4
		syntaxTypeMap.put(21, SyntaxType.INVALID);                    // NAK
		syntaxTypeMap.put(22, SyntaxType.INVALID);                    // SYN
		syntaxTypeMap.put(23, SyntaxType.INVALID);                    // ETB
		syntaxTypeMap.put(24, SyntaxType.INVALID);                    // CAN
		syntaxTypeMap.put(25, SyntaxType.INVALID);                    // EM
		syntaxTypeMap.put(26, SyntaxType.INVALID);                    // SUB
		syntaxTypeMap.put(27, SyntaxType.INVALID);                    // ESC
		syntaxTypeMap.put(28, SyntaxType.INVALID);                    // FS
		syntaxTypeMap.put(29, SyntaxType.INVALID);                    // GS
		syntaxTypeMap.put(30, SyntaxType.INVALID);                    // RS
		syntaxTypeMap.put(31, SyntaxType.INVALID);                    // US
		syntaxTypeMap.put(32, SyntaxType.WHITESPACE);                 // SPACE
		syntaxTypeMap.put(33, SyntaxType.CONSTITUENT);                // !
		syntaxTypeMap.put(34, SyntaxType.TERMINATING);                // "
		syntaxTypeMap.put(35, SyntaxType.NON_TERMINATING);            // #
		syntaxTypeMap.put(36, SyntaxType.CONSTITUENT);                // $
		syntaxTypeMap.put(37, SyntaxType.CONSTITUENT);                // %
		syntaxTypeMap.put(38, SyntaxType.CONSTITUENT);                // &
		syntaxTypeMap.put(39, SyntaxType.TERMINATING);                // '
		syntaxTypeMap.put(40, SyntaxType.TERMINATING);                // (
		syntaxTypeMap.put(41, SyntaxType.TERMINATING);                // )
		syntaxTypeMap.put(42, SyntaxType.CONSTITUENT);                // *
		syntaxTypeMap.put(43, SyntaxType.CONSTITUENT);                // +
		syntaxTypeMap.put(44, SyntaxType.TERMINATING);                // ,
		syntaxTypeMap.put(45, SyntaxType.CONSTITUENT);                // -
		syntaxTypeMap.put(46, SyntaxType.CONSTITUENT);                // .
		syntaxTypeMap.put(47, SyntaxType.CONSTITUENT);                // /
		syntaxTypeMap.put(48, SyntaxType.CONSTITUENT);                // 0
		syntaxTypeMap.put(49, SyntaxType.CONSTITUENT);                // 1
		syntaxTypeMap.put(50, SyntaxType.CONSTITUENT);                // 2
		syntaxTypeMap.put(51, SyntaxType.CONSTITUENT);                // 3
		syntaxTypeMap.put(52, SyntaxType.CONSTITUENT);                // 4
		syntaxTypeMap.put(53, SyntaxType.CONSTITUENT);                // 5
		syntaxTypeMap.put(54, SyntaxType.CONSTITUENT);                // 6
		syntaxTypeMap.put(55, SyntaxType.CONSTITUENT);                // 7
		syntaxTypeMap.put(56, SyntaxType.CONSTITUENT);                // 8
		syntaxTypeMap.put(57, SyntaxType.CONSTITUENT);                // 9
		syntaxTypeMap.put(58, SyntaxType.CONSTITUENT);                // :
		syntaxTypeMap.put(59, SyntaxType.TERMINATING);                // ;
		syntaxTypeMap.put(60, SyntaxType.CONSTITUENT);                // <
		syntaxTypeMap.put(61, SyntaxType.CONSTITUENT);                // =
		syntaxTypeMap.put(62, SyntaxType.CONSTITUENT);                // >
		syntaxTypeMap.put(63, SyntaxType.CONSTITUENT);                // ?
		syntaxTypeMap.put(64, SyntaxType.CONSTITUENT);                // @
		syntaxTypeMap.put(65, SyntaxType.CONSTITUENT);                // A
		syntaxTypeMap.put(66, SyntaxType.CONSTITUENT);                // B
		syntaxTypeMap.put(67, SyntaxType.CONSTITUENT);                // C
		syntaxTypeMap.put(68, SyntaxType.CONSTITUENT);                // D
		syntaxTypeMap.put(69, SyntaxType.CONSTITUENT);                // E
		syntaxTypeMap.put(70, SyntaxType.CONSTITUENT);                // F
		syntaxTypeMap.put(71, SyntaxType.CONSTITUENT);                // G
		syntaxTypeMap.put(72, SyntaxType.CONSTITUENT);                // H
		syntaxTypeMap.put(73, SyntaxType.CONSTITUENT);                // I
		syntaxTypeMap.put(74, SyntaxType.CONSTITUENT);                // J
		syntaxTypeMap.put(75, SyntaxType.CONSTITUENT);                // K
		syntaxTypeMap.put(76, SyntaxType.CONSTITUENT);                // L
		syntaxTypeMap.put(77, SyntaxType.CONSTITUENT);                // M
		syntaxTypeMap.put(78, SyntaxType.CONSTITUENT);                // N
		syntaxTypeMap.put(79, SyntaxType.CONSTITUENT);                // O
		syntaxTypeMap.put(80, SyntaxType.CONSTITUENT);                // P
		syntaxTypeMap.put(81, SyntaxType.CONSTITUENT);                // Q
		syntaxTypeMap.put(82, SyntaxType.CONSTITUENT);                // R
		syntaxTypeMap.put(83, SyntaxType.CONSTITUENT);                // S
		syntaxTypeMap.put(84, SyntaxType.CONSTITUENT);                // T
		syntaxTypeMap.put(85, SyntaxType.CONSTITUENT);                // U
		syntaxTypeMap.put(86, SyntaxType.CONSTITUENT);                // V
		syntaxTypeMap.put(87, SyntaxType.CONSTITUENT);                // W
		syntaxTypeMap.put(88, SyntaxType.CONSTITUENT);                // X
		syntaxTypeMap.put(89, SyntaxType.CONSTITUENT);                // Y
		syntaxTypeMap.put(90, SyntaxType.CONSTITUENT);                // Z
		syntaxTypeMap.put(91, SyntaxType.CONSTITUENT);                // [
		syntaxTypeMap.put(92, SyntaxType.SINGLE_ESCAPE);              // \
		syntaxTypeMap.put(93, SyntaxType.CONSTITUENT);                // ]
		syntaxTypeMap.put(94, SyntaxType.CONSTITUENT);                // ^
		syntaxTypeMap.put(95, SyntaxType.CONSTITUENT);                // _
		syntaxTypeMap.put(96, SyntaxType.TERMINATING);                // `
		syntaxTypeMap.put(97, SyntaxType.CONSTITUENT);                // a
		syntaxTypeMap.put(98, SyntaxType.CONSTITUENT);                // b
		syntaxTypeMap.put(99, SyntaxType.CONSTITUENT);                // c
		syntaxTypeMap.put(100, SyntaxType.CONSTITUENT);               // d
		syntaxTypeMap.put(101, SyntaxType.CONSTITUENT);               // e
		syntaxTypeMap.put(102, SyntaxType.CONSTITUENT);               // f
		syntaxTypeMap.put(103, SyntaxType.CONSTITUENT);               // g
		syntaxTypeMap.put(104, SyntaxType.CONSTITUENT);               // h
		syntaxTypeMap.put(105, SyntaxType.CONSTITUENT);               // i
		syntaxTypeMap.put(106, SyntaxType.CONSTITUENT);               // j
		syntaxTypeMap.put(107, SyntaxType.CONSTITUENT);               // k
		syntaxTypeMap.put(108, SyntaxType.CONSTITUENT);               // l
		syntaxTypeMap.put(109, SyntaxType.CONSTITUENT);               // m
		syntaxTypeMap.put(110, SyntaxType.CONSTITUENT);               // n
		syntaxTypeMap.put(111, SyntaxType.CONSTITUENT);               // o
		syntaxTypeMap.put(112, SyntaxType.CONSTITUENT);               // p
		syntaxTypeMap.put(113, SyntaxType.CONSTITUENT);               // q
		syntaxTypeMap.put(114, SyntaxType.CONSTITUENT);               // r
		syntaxTypeMap.put(115, SyntaxType.CONSTITUENT);               // s
		syntaxTypeMap.put(116, SyntaxType.CONSTITUENT);               // t
		syntaxTypeMap.put(117, SyntaxType.CONSTITUENT);               // u
		syntaxTypeMap.put(118, SyntaxType.CONSTITUENT);               // v
		syntaxTypeMap.put(119, SyntaxType.CONSTITUENT);               // w
		syntaxTypeMap.put(120, SyntaxType.CONSTITUENT);               // x
		syntaxTypeMap.put(121, SyntaxType.CONSTITUENT);               // y
		syntaxTypeMap.put(122, SyntaxType.CONSTITUENT);               // z
		syntaxTypeMap.put(123, SyntaxType.CONSTITUENT);               // {
		syntaxTypeMap.put(124, SyntaxType.MULTIPLE_ESCAPE);           // |
		syntaxTypeMap.put(125, SyntaxType.CONSTITUENT);               // }
		syntaxTypeMap.put(126, SyntaxType.CONSTITUENT);               // ~
		syntaxTypeMap.put(127, SyntaxType.CONSTITUENT);               // DEL
	}

	/**
	 * Gets the matching {@link SyntaxType} for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the {@code codePoint} used to find the matching {@link SyntaxType}
	 *
	 * @return the matching {@link SyntaxType} for the provided {@code codePoint}
	 */
	SyntaxType getSyntaxType(final int codePoint) {
		if (syntaxTypeMap.containsKey(codePoint)) {
			return syntaxTypeMap.get(codePoint);
		}

		if (Character.isDefined(codePoint)) {
			if (Character.isWhitespace(codePoint)) {
				return SyntaxType.WHITESPACE;
			} else if (Character.isUnicodeIdentifierPart(codePoint)
					&& !Character.isIdentifierIgnorable(codePoint)) {
				return SyntaxType.CONSTITUENT;
			} else {
				LOGGER.warn("Defined but illegal: {}", codePoint);
				return SyntaxType.INVALID;
			}
		}

		if (codePoint != CharacterConstants.EXIT_CHAR) {
			LOGGER.error("Not defined and illegal: {}", codePoint);
		}
		return SyntaxType.INVALID;
	}

	/**
	 * Sets the {@link SyntaxType} for the provided {@code codePoint} to the provided {@code syntaxType}.
	 *
	 * @param codePoint
	 * 		the {@code codePoint} to set the {@link SyntaxType}
	 * @param syntaxType
	 * 		the new {@link SyntaxType} value
	 */
	void setSyntaxType(final int codePoint, final SyntaxType syntaxType) {
		syntaxTypeMap.put(codePoint, syntaxType);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
