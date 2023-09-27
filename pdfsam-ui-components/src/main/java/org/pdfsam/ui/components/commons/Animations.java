/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.commons;

import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.scene.Node;
import javafx.util.Duration;

import static javafx.animation.Interpolator.EASE_BOTH;

/**
 * @author Andrea Vacondio
 */
public final class Animations {
    private Animations() {
        // hide
    }

    /**
     * creates a shake animation. This is based on <a href="https://github.com/fxexperience/code/blob/master/FXExperienceControls/src/com/fxexperience/javafx/animation/TadaTransition.java">.<a href="..</a>
     * ">* and http://daneden.gith</a>ub.io/animate.css/
     *
     */
    public static Timeline shake(Node node) {
        Timeline timeline = new Timeline(new KeyFrame(Duration.millis(2500)),
                new KeyFrame(Duration.millis(0), new KeyValue(node.scaleXProperty(), 1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), 0, EASE_BOTH)),
                new KeyFrame(Duration.millis(100), new KeyValue(node.scaleXProperty(), 0.9, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 0.9, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), -3, EASE_BOTH)),
                new KeyFrame(Duration.millis(200), new KeyValue(node.scaleXProperty(), 0.9, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 0.9, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), -3, EASE_BOTH)),
                new KeyFrame(Duration.millis(300), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), 3, EASE_BOTH)),
                new KeyFrame(Duration.millis(400), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), -3, EASE_BOTH)),
                new KeyFrame(Duration.millis(500), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), 3, EASE_BOTH)),
                new KeyFrame(Duration.millis(600), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), -3, EASE_BOTH)),
                new KeyFrame(Duration.millis(700), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), 3, EASE_BOTH)),
                new KeyFrame(Duration.millis(800), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), -3, EASE_BOTH)),
                new KeyFrame(Duration.millis(900), new KeyValue(node.scaleXProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1.1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), 3, EASE_BOTH)),
                new KeyFrame(Duration.millis(1000), new KeyValue(node.scaleXProperty(), 1, EASE_BOTH),
                        new KeyValue(node.scaleYProperty(), 1, EASE_BOTH),
                        new KeyValue(node.rotateProperty(), 0, EASE_BOTH)));
        timeline.setCycleCount(Timeline.INDEFINITE);
        timeline.setDelay(Duration.millis(2000));
        return timeline;
    }
}
