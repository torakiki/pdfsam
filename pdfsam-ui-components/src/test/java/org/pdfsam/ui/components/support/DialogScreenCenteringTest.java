/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15 set 2026
 * Copyright 2026 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.support;

import javafx.geometry.Rectangle2D;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import javafx.stage.Screen;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
@Tag("NoHeadless")
public class DialogScreenCenteringTest {

    private Stage owner;

    @Start
    public void start(Stage stage) {
        this.owner = stage;
        owner.setScene(new Scene(new VBox(), 200, 200));
        owner.show();
    }

    @Test
    public void centeredPositionCentersHorizontallyAndAtOneThirdVertically() {
        var screenBounds = new Rectangle2D(100, 50, 1000, 800);
        var result = DialogScreenCentering.centeredPosition(screenBounds, 300, 200);
        assertThat(result.getX()).isEqualTo(100 + (1000 - 300) / 2.0);
        assertThat(result.getY()).isEqualTo(50 + (800 - 200) / 3.0);
    }

    @Test
    public void centerOnOwnerScreenDoesNothingWhenSizeIsUnknown() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            var dialog = new Stage();
            dialog.initOwner(owner);
            DialogScreenCentering.centerOnOwnerScreen(dialog);
            assertThat(dialog.getX()).isNaN();
            assertThat(dialog.getY()).isNaN();
        });
    }

    @Test
    public void centerOnOwnerScreenOverridesAStalePosition() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            var dialog = new Stage();
            dialog.initOwner(owner);
            dialog.setScene(new Scene(new VBox(), 300, 150));
            // a real show establishes width/height, same as the first time a reused dialog is displayed
            dialog.show();
            dialog.hide();
            // simulate a stale position left over from that previous show, as if it happened on another screen
            dialog.setX(-5000);
            dialog.setY(-5000);

            DialogScreenCentering.centerOnOwnerScreen(dialog);

            var visualBounds = Screen.getPrimary().getVisualBounds();
            var expectedX = visualBounds.getMinX() + (visualBounds.getWidth() - dialog.getWidth()) / 2.0;
            var expectedY = visualBounds.getMinY() + (visualBounds.getHeight() - dialog.getHeight()) / 3.0;
            assertThat(dialog.getX()).isEqualTo(expectedX);
            assertThat(dialog.getY()).isEqualTo(expectedY);
        });
    }
}
