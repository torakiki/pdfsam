/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.basic;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;
import org.pdfsam.core.BrandableProperty;

public class PdfsamBasicTest {
    @Test(expected = IllegalArgumentException.class)
    public void blankName() throws IOException {
        new PdfsamBasic(" ", "something");
    }

    @Test(expected = IllegalArgumentException.class)
    public void blankShortName() throws IOException {
        new PdfsamBasic("Something", " ");
    }

    @Test
    public void property() throws IOException {
        PdfsamBasic pdfsam = new PdfsamBasic("name", "short");
        assertEquals("1.0.0", pdfsam.property(BrandableProperty.VERSION));
        assertEquals("Chuck", pdfsam.property(BrandableProperty.FEED_URL, "Chuck"));
    }
}
