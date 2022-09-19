/*
 * This file is part of the PDF Split And Merge source code
 * Created on 2 gen 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Andrea Vacondio
 */
public class EncryptionUtilsTest {

    private static final String TEST = "Chuck Norris";
    private static final String ENC = "vID7Tz17JKkHsEm2Vf1S1Q==";

    @Test
    public void testEncrypt() {
        assertEquals(ENC, EncryptionUtils.encrypt(TEST));
    }

    @Test
    public void testDecrypt() {
        assertEquals(TEST, EncryptionUtils.decrypt(ENC));
    }

    @Test
    public void nullInput() {
        assertNull(EncryptionUtils.decrypt(null));
        assertNull(EncryptionUtils.encrypt(null));
    }

}
