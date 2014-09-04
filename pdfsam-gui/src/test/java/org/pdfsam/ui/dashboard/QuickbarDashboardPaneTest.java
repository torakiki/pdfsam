/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/set/2014
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
package org.pdfsam.ui.dashboard;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.layout.Pane;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.quickbar.ExpandButton;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class QuickbarDashboardPaneTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Override
    protected Parent getRootNode() {
        ExpandButton expandButton = new ExpandButton();
        expandButton.setId("expand");
        QuickbarDashboardButtons buttons = new QuickbarDashboardButtons(Arrays.asList(new DashboardItem() {

            public int priority() {
                return 0;
            }

            public Pane pane() {
                return null;
            }

            public String name() {
                return "name";
            }

            public String id() {
                return "id";
            }

            public Node graphic() {
                return null;
            }
        }));
        buttons.setId("buttons");
        return new QuickbarDashboardPane(expandButton, buttons);
    }

    @Test
    public void click() {
        QuickbarDashboardButtons buttons = find("#buttons");
        assertFalse(buttons.isDisplayText());
        click("#expand");
        assertTrue(buttons.isDisplayText());
    }
}
