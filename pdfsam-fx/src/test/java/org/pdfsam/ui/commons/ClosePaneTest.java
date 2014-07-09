/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/lug/2014
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
import javafx.scene.layout.HBox;

import org.junit.Test;
import org.loadui.testfx.GuiTest;

/**
 * @author Andrea Vacondio
 *
 */
public class ClosePaneTest extends GuiTest {

    @Override
    protected Parent getRootNode() {
        return new ClosePane();
    }

    @Test
    public void hide() {
        verifyThat(".pdfsam-container", (HBox n) -> n.getScene().getWindow().isShowing());
        click(".pdfsam-button");
        verifyThat(".pdfsam-container", (HBox n) -> !n.getScene().getWindow().isShowing());
    }
}
