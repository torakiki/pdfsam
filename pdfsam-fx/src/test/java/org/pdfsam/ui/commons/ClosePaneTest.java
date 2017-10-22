/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/lug/2014
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
package org.pdfsam.ui.commons;

import static org.loadui.testfx.Assertions.verifyThat;

import org.apache.commons.lang3.SystemUtils;
import org.junit.Assume;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;

import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class ClosePaneTest extends GuiTest {
    private Stage victimStage;

    @Override
    protected Parent getRootNode() {
        victimStage = new Stage();
        ClosePane containerPane = new ClosePane();
        Scene scene = new Scene(containerPane);
        victimStage.setScene(scene);
        Button button = new Button("show");
        button.setOnAction(a -> victimStage.show());
        return button;
    }

    @Test
    public void hide() {
        Assume.assumeTrue(!SystemUtils.IS_OS_WINDOWS);
        click("show");
        verifyThat(".pdfsam-container", (HBox n) -> n.getScene().getWindow().isShowing());
        click(".pdfsam-button");
        verifyThat(".pdfsam-container", (HBox n) -> !n.getScene().getWindow().isShowing());
    }
}
