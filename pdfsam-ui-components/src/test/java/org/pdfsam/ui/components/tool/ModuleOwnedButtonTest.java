/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/lug/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.ui.components.tool;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.JavaFxThreadExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ JavaFxThreadExtension.class })
public class ModuleOwnedButtonTest {

    @Test
    public void nullArg() {
        ToolBoundButton victim = new ToolBoundButton(null);
        assertNotNull(victim.toolBinding());
        assertEquals(StringUtils.EMPTY, victim.toolBinding());
    }

    @Test
    public void notNullArg() {
        ToolBoundButton victim = new ToolBoundButton("Chuck");
        assertEquals("Chuck", victim.toolBinding());
    }
}
