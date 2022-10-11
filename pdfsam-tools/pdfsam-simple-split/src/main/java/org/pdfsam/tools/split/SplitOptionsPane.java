/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
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
package org.pdfsam.tools.split;

import javafx.scene.control.ComboBox;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.VBox;
import org.pdfsam.core.support.params.SinglePdfSourceMultipleOutputParametersBuilder;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.commons.RadioButtonDrivenTextFieldsPane;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.parameter.AbstractSplitByPageParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.help.HelpUtils.helpIcon;

/**
 * Panel for the Split options
 *
 * @author Andrea Vacondio
 */
class SplitOptionsPane extends VBox implements SplitParametersBuilderCreator, RestorableView, ResettableView {

    private final ToggleGroup group = new ToggleGroup();
    private final SplitAfterPredefinedSetOfPagesRadioButton splitAfterPredefined;
    private final SplitAfterRadioButton splitAfter;
    private final SplitByEveryRadioButton splitByEvery;

    SplitOptionsPane() {
        super(Style.DEFAULT_SPACING);
        var predefinedCombo = new ComboBox<ComboItem<PredefinedSetOfPages>>();
        predefinedCombo.getItems().add(new ComboItem<>(PredefinedSetOfPages.ALL_PAGES, i18n().tr("Every page")));
        predefinedCombo.getItems().add(new ComboItem<>(PredefinedSetOfPages.EVEN_PAGES, i18n().tr("Even pages")));
        predefinedCombo.getItems().add(new ComboItem<>(PredefinedSetOfPages.ODD_PAGES, i18n().tr("Odd pages")));
        splitAfterPredefined = new SplitAfterPredefinedSetOfPagesRadioButton(predefinedCombo);
        ValidableTextField splitAfterField = new ValidableTextField();
        splitAfter = new SplitAfterRadioButton(splitAfterField);
        ValidableTextField splitByEveryField = new ValidableTextField();
        splitByEvery = new SplitByEveryRadioButton(splitByEveryField);
        RadioButtonDrivenTextFieldsPane grid = new RadioButtonDrivenTextFieldsPane(group);

        splitAfterPredefined.setToggleGroup(group);
        splitAfter.setToggleGroup(group);
        splitByEvery.setToggleGroup(group);
        grid.addRow(splitAfterPredefined, predefinedCombo,
                helpIcon(i18n().tr("Split the document after the given page numbers")));
        grid.addRow(splitAfter, splitAfterField,
                helpIcon(i18n().tr("Split the document after the given page numbers")));
        grid.addRow(splitByEvery, splitByEveryField,
                helpIcon(i18n().tr("Splits the PDF every \"n\" pages creating documents of \"n\" pages each")));
        splitAfterPredefined.setSelected(true);

        getStyleClass().addAll(Style.CONTAINER.css());
        getChildren().addAll(grid);
    }

    void setMaxPages(Integer value) {
        splitByEvery.setMaxPages(value);
    }

    @Override
    public SinglePdfSourceMultipleOutputParametersBuilder<? extends AbstractSplitByPageParameters> getBuilder(
            Consumer<String> onError) {
        return ((SplitParametersBuilderCreator) group.getSelectedToggle()).getBuilder(onError);
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        splitAfterPredefined.saveStateTo(data);
        splitAfter.saveStateTo(data);
        splitByEvery.saveStateTo(data);
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        splitAfterPredefined.restoreStateFrom(data);
        splitAfter.restoreStateFrom(data);
        splitByEvery.restoreStateFrom(data);
    }

    @Override
    public void resetView() {
        splitAfterPredefined.resetView();
        splitAfter.resetView();
        splitByEvery.resetView();
        splitAfterPredefined.setSelected(true);
    }
}
