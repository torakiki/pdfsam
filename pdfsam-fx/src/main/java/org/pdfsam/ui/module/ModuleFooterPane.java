/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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
package org.pdfsam.ui.module;

import javafx.scene.layout.HBox;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.ui.support.Style;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;

/**
 * Footer common to all the modules that include the run button and the progress bar.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
@Scope(BeanDefinition.SCOPE_PROTOTYPE)
class ModuleFooterPane extends HBox {

    private RunButton runButton = new RunButton();

    @Inject
    public ModuleFooterPane(ProgressPane progress) {
        this.getStyleClass().addAll(Style.CLOSE_FOOTER.css());
        this.getChildren().addAll(progress, runButton);
    }

    RunButton runButton() {
        return runButton;
    }
}
