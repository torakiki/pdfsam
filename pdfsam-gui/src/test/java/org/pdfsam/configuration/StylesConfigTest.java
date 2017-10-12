/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.configuration;

import static org.junit.Assert.assertFalse;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.Theme;

/**
 * @author Andrea Vacondio
 *
 */
public class StylesConfigTest {

    @Rule
    public InitializeAndApplyJavaFxThreadRule initFx = new InitializeAndApplyJavaFxThreadRule();

    @SuppressWarnings("unused")
    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        new StylesConfig(null);
    }

    @Test
    public void nonNullArg() {
        StylesConfig victim = new StylesConfig(Theme.ROUNDISH);
        assertFalse(victim.styles().isEmpty());
    }
}
