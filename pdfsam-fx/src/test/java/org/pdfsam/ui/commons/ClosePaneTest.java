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

import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import static org.testfx.api.FxAssert.verifyThat;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.test.HitTestListener;
import org.testfx.framework.junit.ApplicationTest;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class ClosePaneTest extends ApplicationTest {
    private Stage victimStage;

    @Override
    public void start(Stage stage) {
        victimStage = new Stage();
        Button button = new Button("show");
        button.setOnAction(a -> victimStage.show());
        Scene scene = new Scene(new HBox(button));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Category(NoHeadless.class)
    public void hide() {
        ClosePane containerPane = new ClosePane();
        Scene scene = new Scene(containerPane);
        Platform.runLater(() -> victimStage.setScene(scene));
        clickOn("show");
        assertTrue(robotContext().getWindowFinder().listWindows().size() > 1);
        clickOn(".pdfsam-button");
        assertTrue(robotContext().getWindowFinder().listWindows().size() == 1);
    }

    @Test
    @Category(NoHeadless.class)
    public void customAction() {
        ClosePane containerPane = new ClosePane(k -> eventStudio().broadcast(HideStageRequest.INSTANCE));
        Scene scene = new Scene(containerPane);
        Platform.runLater(() -> victimStage.setScene(scene));
        clickOn("show");
        verifyThat(".pdfsam-container", (HBox n) -> n.getScene().getWindow().isShowing());
        HitTestListener<HideStageRequest> listener = new HitTestListener<>();
        eventStudio().add(HideStageRequest.class, listener);
        clickOn(".pdfsam-button");
        assertTrue(listener.isHit());
    }
}
