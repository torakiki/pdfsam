/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/lug/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.commons;

import static org.loadui.testfx.Assertions.verifyThat;
import javafx.scene.Parent;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class HidingPaneTest extends GuiTest {
    private HidingPane victim;

    @Override
    protected Parent getRootNode() {
        victim = new HidingPane();
        return victim;
    }

    @Test
    public void hide() {
        verifyThat(victim, (v) -> v.isVisible());
        click(".pdfsam-button");
        verifyThat(victim, (v) -> !v.isVisible());
    }

}
