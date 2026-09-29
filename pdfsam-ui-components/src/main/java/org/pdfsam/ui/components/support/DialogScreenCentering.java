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

import javafx.geometry.Point2D;
import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.Window;

/**
 * JavaFX centers an owned {@link Stage} only the first time it's shown, on the screen currently hosting its owner. Once shown, the stage
 * keeps its last known position on later {@code show()} calls, even if the owner has since moved to a different screen. Dialogs that are
 * created once and reused would otherwise stay pinned to whatever screen they first appeared on.
 *
 * @author Andrea Vacondio
 */
public final class DialogScreenCentering {

    // same fractions JavaFX itself uses in Window#centerOnScreen, kept in sync so a reused dialog doesn't jump
    // vertically between its first, JavaFX-driven centering and the later re-centering done here
    private static final double CENTER_X_FRACTION = 1.0 / 2;
    private static final double CENTER_Y_FRACTION = 1.0 / 3;

    private DialogScreenCentering() {
        // utility
    }

    /**
     * Re-centers the given stage on the screen currently hosting its owner, overriding the position it kept from the previous time it
     * was shown. Does nothing while the stage's size isn't known yet, i.e. before it has ever been shown, since JavaFX already centers it
     * correctly on the owner's screen in that case.
     *
     * @param stage the reused, owned stage to re-center
     */
    public static void centerOnOwnerScreen(Stage stage) {
        var owner = stage.getOwner();
        if (owner != null && !Double.isNaN(stage.getWidth()) && !Double.isNaN(stage.getHeight())) {
            var target = centeredPosition(ownerScreenVisualBounds(owner), stage.getWidth(), stage.getHeight());
            stage.setX(target.getX());
            stage.setY(target.getY());
        }
    }

    /**
     * @return the visual bounds of the screen currently hosting the given window, picked using the window's center point
     */
    private static Rectangle2D ownerScreenVisualBounds(Window owner) {
        double centerX = owner.getX() + owner.getWidth() / 2;
        double centerY = owner.getY() + owner.getHeight() / 2;
        return Screen.getScreensForRectangle(centerX, centerY, 1, 1).stream().findFirst()
                .orElseGet(Screen::getPrimary).getVisualBounds();
    }

    static Point2D centeredPosition(Rectangle2D screenVisualBounds, double width, double height) {
        double x = screenVisualBounds.getMinX() + (screenVisualBounds.getWidth() - width) * CENTER_X_FRACTION;
        double y = screenVisualBounds.getMinY() + (screenVisualBounds.getHeight() - height) * CENTER_Y_FRACTION;
        return new Point2D(x, y);
    }
}
