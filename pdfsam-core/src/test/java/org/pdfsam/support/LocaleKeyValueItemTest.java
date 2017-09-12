/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/lug/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static org.junit.Assert.assertEquals;

import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.pdfsam.TestUtils;

/**
 * @author Andrea Vacondio
 *
 */
public class LocaleKeyValueItemTest {

    @Test(expected = IllegalArgumentException.class)
    public void nullConstructor() {
        new LocaleKeyValueItem(null);
    }

    @Test
    public void validValue() {
        LocaleKeyValueItem victim = new LocaleKeyValueItem(Locale.ITALY);
        assertEquals(StringUtils.capitalize(Locale.ITALY.getDisplayName()), victim.getValue());
        assertEquals(Locale.ITALY.toLanguageTag(), victim.getKey());
    }

    @Test
    public void equalsAndHashCodes() {
        LocaleKeyValueItem eq1 = new LocaleKeyValueItem(Locale.CANADA);
        LocaleKeyValueItem eq2 = new LocaleKeyValueItem(Locale.CANADA);
        LocaleKeyValueItem eq3 = new LocaleKeyValueItem(Locale.CANADA);
        LocaleKeyValueItem diff = new LocaleKeyValueItem(Locale.CHINA);
        TestUtils.testEqualsAndHashCodes(eq1, eq2, eq3, diff);
    }
}
