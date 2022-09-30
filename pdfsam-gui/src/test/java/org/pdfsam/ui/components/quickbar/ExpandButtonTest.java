/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ago/2014
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
package org.pdfsam.ui.components.quickbar;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.testfx.framework.junit.ApplicationTest;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.ToggleButton;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class ExpandButtonTest extends ApplicationTest {

    @Override
    public void start(Stage stage) {
        Scene scene = new Scene(new ExpandButton());
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void changeGraphic() {
        ToggleButton toggle = lookup(".quickbar-expand-toggle").queryAs(ToggleButton.class);
        ChangeListener<? super Node> listener = mock(ChangeListener.class);
        toggle.graphicProperty().addListener(listener);
        clickOn(".quickbar-expand-toggle");
        verify(listener).changed(any(ObservableValue.class), any(Node.class), any(Node.class));
    }
}
