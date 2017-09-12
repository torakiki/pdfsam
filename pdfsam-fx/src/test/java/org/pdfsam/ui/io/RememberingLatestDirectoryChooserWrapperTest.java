/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/lug/2014
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
package org.pdfsam.ui.io;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;

import javafx.scene.Parent;
import javafx.scene.control.Button;

/**
 * @author Andrea Vacondio
 *
 */
@Ignore
@Category(TestFX.class)
public class RememberingLatestDirectoryChooserWrapperTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Test
    public void hasTitle() {
        click(".aButton");
        sleep(1000);
        // TODO
    }

    @Override
    protected Parent getRootNode() {
        RememberingLatestDirectoryChooserWrapper victim = new RememberingLatestDirectoryChooserWrapper();
        victim.setTitle("Browse folders");
        Button button = new Button("Click me");
        button.getStyleClass().add("aButton");
        button.setOnAction(e -> victim.showDialog(button.getScene().getWindow()));
        return button;
    }
}
